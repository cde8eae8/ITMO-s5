import org.jetbrains.annotations.NotNull;

import static java.lang.Math.max;

/**
 * В теле класса решения разрешено использовать только финальные переменные типа RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author : Дроздов Никита
 */
public class Solution implements MonotonicClock {
    private final RegularInt c1 = new RegularInt(0);
    private final RegularInt c2 = new RegularInt(0);
    private final RegularInt c3 = new RegularInt(0);

    private final RegularInt t1 = new RegularInt(0);
    private final RegularInt t2 = new RegularInt(0);
    private final RegularInt t3 = new RegularInt(0);

    private final RegularInt l1 = new RegularInt(0);
    private final RegularInt l2 = new RegularInt(0);
    private final RegularInt l3 = new RegularInt(0);


    @Override
    public void write(@NotNull Time time) {
        // t >= c
        // t
        // t1, t2, t3
        //    --->
        // c1, c2, c3
        //    <---
        // write left-to-right
        t1.setValue(time.getD1());
        t2.setValue(time.getD2());
        t3.setValue(time.getD3());
        // write right-to-left
        c3.setValue(time.getD3());
        c2.setValue(time.getD2());
        c1.setValue(time.getD1());
    }

    @NotNull
    @Override
    public Time read() {
        Time t = readImpl();
        Time l = new Time(l1.getValue(), l2.getValue(), l3.getValue());
        if (t.compareTo(l) > 0) {
            l1.setValue(t.component1());
            l2.setValue(t.component2());
            l3.setValue(t.component3());
        }
        l = new Time(l1.getValue(), l2.getValue(), l3.getValue());
        return l;
    }

    private Time readImpl() {
        // read left-to-right
        int lc1, lc2, lc3;
        lc1 = c1.getValue();
        lc2 = c2.getValue();
        lc3 = c3.getValue();
        Time ct = new Time(lc1, lc2, lc3);
        int lt1, lt2, lt3;
        lt3 = t3.getValue();
        lt2 = t2.getValue();
        lt1 = t1.getValue();
        Time tt = new Time(lt1, lt2, lt3);
        if (tt.getD1() != ct.getD1())
            return new Time(tt.getD1(), 0, 0);
        else if (tt.getD2() != ct.getD2())
            return new Time(tt.getD1(), tt.getD2(), 0);
        else return tt;
        //else if (tt.getD3() != ct.getD3())
        //    return new Time(tt.getD1(), tt.getD2(), tt.getD3());
    }
}
