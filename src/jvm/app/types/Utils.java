package app.types;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.ZonedDateTime;
import java.time.ZoneId;
import org.agrona.DirectBuffer;
import org.agrona.MutableDirectBuffer;
import org.agrona.concurrent.UnsafeBuffer;
import org.apache.commons.collections4.map.LRUMap;
import org.apache.commons.lang3.StringUtils;

public final class Utils {
  public static ZoneId zoneId = ZoneId.of("US/Eastern");

  private Utils() {}

  private static LRUMap<String, Instant> stringToInstantCache =
      new LRUMap<String, Instant>(1024 * 1024);
  private static DateTimeFormatter dateTimeFormatter =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

  public static Instant QuoteDateTimeToInstant(String s) {
    if (stringToInstantCache.containsKey(s)) {
      return stringToInstantCache.get(s);
    }

    final var t = LocalDateTime.parse(s, dateTimeFormatter).toInstant(ZoneOffset.UTC);
    stringToInstantCache.put(s, t);
    return t;
  }

  ////////////////////////////////////////////////////////////////////////////////

  public static int ExpDateToInt(String s) {
    final var year = Integer.parseInt(s.substring(0, 4));
    final var month = Integer.parseInt(s.substring(5, 7));
    final var day = Integer.parseInt(s.substring(8, 10));
    return day + (100 * month) + (10000 * year);
  }

  private static LRUMap<Integer, String> expDateIntToString = new LRUMap<Integer, String>(1024 * 1024);
  public static String IntToExpDate(int i) {
    if (expDateIntToString.containsKey(i)) {
      return expDateIntToString.get(i);
    }
    
    final String day = StringUtils.leftPad(String.valueOf(i % 100), 2, "0");
    i = i / 100;
    final String month = StringUtils.leftPad(String.valueOf(i % 100), 2, "0");
    i = i / 100;
    final String year = String.valueOf(i);
    final var s = year + "-" + month + "-" + day;
    expDateIntToString.put(i, s);
    return s;
  }

  public static ByteBuffer InstantToByteBuffer(Instant t) {
    final long sec = t.getEpochSecond();
    final var buf = ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN).putLong(sec);
    return buf;
  }

  public static Instant ByteBufferToInstant(DirectBuffer srcBuf, int i) {
    final var dstBuf = ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN);
    srcBuf.getBytes(i, dstBuf, 4, 4);
    final long sec = dstBuf.getLong(0);
    return Instant.ofEpochSecond(sec);
  }

  private static final char[] HEX_ARRAY = "0123456789ABCDEF".toCharArray();

  public static String DirectBufferToHexString(DirectBuffer src) {
    final byte[] bytes = new byte[src.capacity()];
    src.getBytes(0, bytes);
    char[] hexChars = new char[bytes.length * 2];
    for (int j = 0; j < bytes.length; j++) {
      int v = bytes[j] & 0xFF;
      hexChars[j * 2] = HEX_ARRAY[v >>> 4];
      hexChars[j * 2 + 1] = HEX_ARRAY[v & 0x0F];
    }
    return new String(hexChars);
  }

  ////////////////////////////////////////////////////////////////////////////////

  public static float ParsePriceString(String s) {
    // final var n = s.length();
    // final char ch = s.charAt(n - 3);
    // if (ch != '.') {
    //   throw new RuntimeException("third to last char must be a period");
    // }
    // final String a = s.substring(0, n - 3);
    // final String b = s.substring(n - 2, n);
    // return Integer.parseInt(a + b);

    // final var f = Float.parseFloat(s) * 100;
    // return (int)Math.floor(f);

    return Float.parseFloat(s);
  }
}
