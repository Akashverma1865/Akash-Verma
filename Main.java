package src;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Main {

    // ===== Minimal JSON Parser (objects + strings + numbers) =====
    private static abstract class JsonValue {}
    private static final class JsonObject extends JsonValue {
        final Map<String, JsonValue> map = new HashMap<>();
    }
    private static final class JsonString extends JsonValue {
        final String value;
        JsonString(String v) { this.value = v; }
    }
    private static final class JsonNumber extends JsonValue {
        final String raw; // keep as string; we parse where needed
        JsonNumber(String r) { this.raw = r; }
    }

    private static final class JsonParser {
        private final String s;
        private int i = 0;
        JsonParser(String s) { this.s = s; }

        JsonValue parse() {
            skipWs();
            JsonValue v = parseValue();
            skipWs();
            if (i != s.length()) throw new RuntimeException("Extra data after JSON end at pos " + i);
            return v;
        }

        private JsonValue parseValue() {
            skipWs();
            if (i >= s.length()) throw new RuntimeException("Unexpected end of input");
            char c = s.charAt(i);
            if (c == '{') return parseObject();
            if (c == '"') return new JsonString(parseString());
            if (c == '-' || Character.isDigit(c)) return parseNumber();
            throw new RuntimeException("Unsupported JSON value at pos " + i + ": '" + c + "'");
        }

        private JsonObject parseObject() {
            expect('{');
            JsonObject obj = new JsonObject();
            skipWs();
            if (peek('}')) { expect('}'); return obj; }
            while (true) {
                skipWs();
                if (s.charAt(i) != '"') throw new RuntimeException("Expected string key at pos " + i);
                String key = parseString();
                skipWs();
                expect(':');
                skipWs();
                JsonValue val = parseValue();
                obj.map.put(key, val);
                skipWs();
                if (peek('}')) { expect('}'); break; }
                expect(',');
            }
            return obj;
        }

        private String parseString() {
            expect('"');
            StringBuilder sb = new StringBuilder();
            while (i < s.length()) {
                char c = s.charAt(i++);
                if (c == '"') break;
                if (c == '\\') {
                    if (i >= s.length()) throw new RuntimeException("Bad escape at end");
                    char e = s.charAt(i++);
                    switch (e) {
                        case '"': sb.append('"'); break;
                        case '\\': sb.append('\\'); break;
                        case '/': sb.append('/'); break;
                        case 'b': sb.append('\b'); break;
                        case 'f': sb.append('\f'); break;
                        case 'n': sb.append('\n'); break;
                        case 'r': sb.append('\r'); break;
                        case 't': sb.append('\t'); break;
                        case 'u': {
                            if (i + 4 > s.length()) throw new RuntimeException("Bad unicode escape at pos " + (i - 2));
                            int cp = Integer.parseInt(s.substring(i, i + 4), 16);
                            sb.append((char) cp);
                            i += 4;
                            break;
                        }
                        default: throw new RuntimeException("Invalid escape \\" + e + " at pos " + (i - 1));
                    }
                } else {
                    sb.append(c);
                }
            }
            return sb.toString();
        }

        private JsonNumber parseNumber() {
            int start = i;
            if (s.charAt(i) == '-') i++;
            while (i < s.length() && Character.isDigit(s.charAt(i))) i++;
            // optional fraction/exponent are ignored for this problem; inputs are integers
            return new JsonNumber(s.substring(start, i));
        }

        private void skipWs() {
            while (i < s.length()) {
                char c = s.charAt(i);
                if (c == ' ' || c == '\n' || c == '\r' || c == '\t') i++;
                else break;
            }
        }

        private boolean peek(char c) { return i < s.length() && s.charAt(i) == c; }
        private void expect(char c) {
            if (i >= s.length() || s.charAt(i) != c) throw new RuntimeException("Expected '" + c + "' at pos " + i);
            i++;
        }
    }

    // ===== Rational arithmetic with BigInteger =====
    private static final class Rational {
        final BigInteger num; // can be negative
        final BigInteger den; // always positive

        Rational(BigInteger num, BigInteger den) {
            if (den.signum() == 0) throw new ArithmeticException("Denominator is zero");
            // normalize sign to denominator positive
            if (den.signum() < 0) {
                num = num.negate();
                den = den.negate();
            }
            BigInteger g = num.gcd(den);
            if (!g.equals(BigInteger.ONE)) {
                num = num.divide(g);
                den = den.divide(g);
            }
            this.num = num;
            this.den = den;
        }

        static Rational of(long v) { return new Rational(BigInteger.valueOf(v), BigInteger.ONE); }
        static Rational of(BigInteger v) { return new Rational(v, BigInteger.ONE); }

        Rational add(Rational o) {
            BigInteger n = this.num.multiply(o.den).add(o.num.multiply(this.den));
            BigInteger d = this.den.multiply(o.den);
            return new Rational(n, d);
        }

        Rational subtract(Rational o) {
            BigInteger n = this.num.multiply(o.den).subtract(o.num.multiply(this.den));
            BigInteger d = this.den.multiply(o.den);
            return new Rational(n, d);
        }

        Rational multiply(Rational o) {
            BigInteger n = this.num.multiply(o.num);
            BigInteger d = this.den.multiply(o.den);
            return new Rational(n, d);
        }

        Rational divide(Rational o) {
            if (o.num.signum() == 0) throw new ArithmeticException("Division by zero");
            BigInteger n = this.num.multiply(o.den);
            BigInteger d = this.den.multiply(o.num);
            return new Rational(n, d);
        }

        boolean isInteger() { return den.equals(BigInteger.ONE); }
        BigInteger asIntegerExact() {
            if (!isInteger()) throw new ArithmeticException("Rational is not an integer: " + this);
            return num;
        }

        @Override public String toString() { return isInteger() ? num.toString() : (num + "/" + den); }

        @Override public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Rational)) return false;
            Rational r = (Rational) o;
            return num.equals(r.num) && den.equals(r.den);
        }

        @Override public int hashCode() {
            return num.hashCode() * 31 + den.hashCode();
        }
    }

    private static final class Share {
        final int x;
        final int base;
        final String valueStr;
        final BigInteger y; // parsed value
        Share(int x, int base, String valueStr, BigInteger y) {
            this.x = x; this.base = base; this.valueStr = valueStr; this.y = y;
        }
        @Override public String toString() {
            return "{" + x + ": base=" + base + ", value=\"" + valueStr + "\" (dec=" + y + ")}";
        }
    }

    public static void main(String[] args) throws Exception {
        String json = readAllStdIn();
        JsonParser parser = new JsonParser(json);
        JsonValue rootV = parser.parse();
        if (!(rootV instanceof JsonObject)) throw new RuntimeException("Root must be a JSON object");
        JsonObject root = (JsonObject) rootV;

        // Extract keys.n and keys.k
        JsonObject keysObj = getObj(root, "keys");
        int n = getInt(keysObj, "n");
        int k = getInt(keysObj, "k");

        // Extract shares: entries other than "keys"
        List<Share> shares = new ArrayList<>();
        for (Map.Entry<String, JsonValue> e : root.map.entrySet()) {
            String key = e.getKey();
            if ("keys".equals(key)) continue;
            int x;
            try { x = Integer.parseInt(key); }
            catch (NumberFormatException ex) { continue; }
            if (!(e.getValue() instanceof JsonObject)) continue;
            JsonObject sObj = (JsonObject) e.getValue();
            String baseStr = getString(sObj, "base");
            String valueStr = getString(sObj, "value");
            int base = Integer.parseInt(baseStr);
            BigInteger y = parseInBase(valueStr, base);
            shares.add(new Share(x, base, valueStr, y));
        }

        if (shares.size() != n) {
            // Not fatal, but warn in output
        }

        // Compute secrets from all k-combinations using Lagrange at x=0
        Map<Rational, Integer> freq = new HashMap<>();
        List<int[]> combs = new ArrayList<>();
        int[] idxs = new int[k];
        generateCombinations(shares.size(), k, 0, 0, idxs, combs);

        Map<Rational, List<int[]>> secretToCombos = new HashMap<>();
        for (int[] comb : combs) {
            Rational secret = interpolateConstantAtZero(shares, comb);
            freq.put(secret, freq.getOrDefault(secret, 0) + 1);
            secretToCombos.computeIfAbsent(secret, t -> new ArrayList<>()).add(Arrays.copyOf(comb, comb.length));
        }

        if (freq.isEmpty()) {
            System.out.println("No combinations to evaluate.");
            return;
        }

        // Choose the most frequent secret (mode). Prefer integer results in tie-breakers.
        Rational bestSecret = null;
        int bestCount = -1;
        for (Map.Entry<Rational, Integer> e : freq.entrySet()) {
            Rational r = e.getKey();
            int c = e.getValue();
            if (c > bestCount) {
                bestCount = c; bestSecret = r;
            } else if (c == bestCount) {
                // tie-breaker: prefer integer
                if (bestSecret != null && !bestSecret.isInteger() && r.isInteger()) {
                    bestSecret = r;
                }
            }
        }

        // Determine consistent shares: any share index that appears in at least one winning combination
        Set<Integer> consistentIndices = new HashSet<>();
        List<int[]> winning = secretToCombos.getOrDefault(bestSecret, Collections.emptyList());
        for (int[] comb : winning) for (int id : comb) consistentIndices.add(id);

        // Prepare outputs
        System.out.println("k = " + k + ", n = " + n);
        System.out.println("Total shares provided = " + shares.size());
        System.out.println("Total combinations checked = " + combs.size());
        System.out.println("Most frequent secret (f(0)) = " + bestSecret + (bestSecret.isInteger() ? "" : "  [non-integer]") );

        // Report consistent and inconsistent shares by participant id (their x)
        List<Share> sharesSorted = new ArrayList<>(shares);
        sharesSorted.sort(Comparator.comparingInt(s1 -> s1.x));

        System.out.println();
        System.out.println("Consistent shares (supporting the chosen secret):");
        for (int i = 0; i < sharesSorted.size(); i++) {
            Share sh = sharesSorted.get(i);
            if (consistentIndices.contains(indexOfShare(shares, sh))) {
                System.out.println("- participant " + sh.x + ": base=" + sh.base + ", value=\"" + sh.valueStr + "\"");
            }
        }

        System.out.println();
        System.out.println("Inconsistent shares (likely wrong):");
        boolean anyBad = false;
        for (int i = 0; i < sharesSorted.size(); i++) {
            Share sh = sharesSorted.get(i);
            if (!consistentIndices.contains(indexOfShare(shares, sh))) {
                anyBad = true;
                System.out.println("- participant " + sh.x + ": base=" + sh.base + ", value=\"" + sh.valueStr + "\"");
            }
        }
        if (!anyBad) System.out.println("- none");
    }

    private static int indexOfShare(List<Share> shares, Share sh) {
        for (int i = 0; i < shares.size(); i++) {
            if (shares.get(i) == sh) return i;
        }
        return -1;
    }

    private static Rational interpolateConstantAtZero(List<Share> shares, int[] comb) {
        Rational sum = Rational.of(0);
        for (int a = 0; a < comb.length; a++) {
            Share si = shares.get(comb[a]);
            Rational li0 = Rational.of(1);
            for (int b = 0; b < comb.length; b++) {
                if (a == b) continue;
                Share sj = shares.get(comb[b]);
                BigInteger num = BigInteger.valueOf(-sj.x);
                BigInteger den = BigInteger.valueOf(si.x - sj.x);
                li0 = li0.multiply(new Rational(num, den));
            }
            sum = sum.add(li0.multiply(Rational.of(si.y)));
        }
        return sum;
    }

    private static void generateCombinations(int n, int k, int start, int depth, int[] cur, List<int[]> out) {
        if (depth == k) {
            out.add(Arrays.copyOf(cur, k));
            return;
        }
        for (int i = start; i <= n - (k - depth); i++) {
            cur[depth] = i;
            generateCombinations(n, k, i + 1, depth + 1, cur, out);
        }
    }

    private static BigInteger parseInBase(String value, int base) {
        if (base < 2 || base > 16) throw new IllegalArgumentException("Unsupported base: " + base);
        BigInteger res = BigInteger.ZERO;
        BigInteger b = BigInteger.valueOf(base);
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            int digit = charToDigit(c);
            if (digit < 0 || digit >= base) throw new IllegalArgumentException("Digit '" + c + "' out of range for base " + base);
            res = res.multiply(b).add(BigInteger.valueOf(digit));
        }
        return res;
    }

    private static int charToDigit(char c) {
        if (c >= '0' && c <= '9') return c - '0';
        if (c >= 'a' && c <= 'f') return 10 + (c - 'a');
        if (c >= 'A' && c <= 'F') return 10 + (c - 'A');
        throw new IllegalArgumentException("Invalid digit: " + c);
    }

    private static JsonObject getObj(JsonObject obj, String key) {
        JsonValue v = obj.map.get(key);
        if (!(v instanceof JsonObject)) throw new RuntimeException("Expected object for key '" + key + "'");
        return (JsonObject) v;
    }

    private static String getString(JsonObject obj, String key) {
        JsonValue v = obj.map.get(key);
        if (v instanceof JsonString) return ((JsonString) v).value;
        if (v instanceof JsonNumber) return ((JsonNumber) v).raw; // allow numbers as strings
        throw new RuntimeException("Expected string/number for key '" + key + "'");
    }

    private static int getInt(JsonObject obj, String key) {
        JsonValue v = obj.map.get(key);
        if (v instanceof JsonNumber) return Integer.parseInt(((JsonNumber) v).raw);
        if (v instanceof JsonString) return Integer.parseInt(((JsonString) v).value);
        throw new RuntimeException("Expected int for key '" + key + "'");
    }

    private static String readAllStdIn() throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringBuilder sb = new StringBuilder();
        String line;
        while ((line = br.readLine()) != null) {
            sb.append(line).append('\n');
        }
        return sb.toString();
    }
}


