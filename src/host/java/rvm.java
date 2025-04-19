import java.io.IOException;
import java.util.Arrays;

class RVM {
    public static int pos = -1;
    static Object stack = new Object[]{0, 0,0};
    static Object[] FALSE = new Object[]{0,0,5,0};
    static Object[] TRUE = new Object[]{0,0,5,1};
    static Object[] NIL = new Object[]{0,0,5};
    static Object x;
    static Object y;
    static Object z;
    static Object[] symtbl;
    public static void main(String[] args) {


        // @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
        String input = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y";
        // )@@
        
        symtbl = NIL;
        int n = get_int(0,input);
        while (n > 0){
            n-=1;
            symtbl = new Object[]{new Object[]{FALSE,new Object[]{NIL,0,3},2},symtbl,0};
        }
        Object accum = NIL;
        n = 0;
        Object nArray = null;
        while (true){
            int c = get_byte(input);
            if (c == 44){
                symtbl = new Object[]{new Object[]{FALSE,new Object[]{accum,n,3},2},symtbl,0};accum=NIL;n=0;
            }else {
                if (c==59){
                    Object[]symbol_to_append = new Object[]{FALSE,new Object[]{accum,n,3},2};
                    symtbl = new Object[]{symbol_to_append,symtbl,0};
                    break;
                }
                accum = new Object[]{c,accum,0};
                n++;
            }
        }
        int[] temp_num = {20,30,0,10,11,4};
        while(true){
            int x = get_Code(input);
            n = x;
            int d = 0;
            int op = 0;
            while (true){
                d = temp_num[op];
                if (n <=2+d)break;
                n-=d+3;op+=1;
            }
            if (x > 90) nArray = pop();
            else{
                if (op ==0) {
                    stack = new Object[]{0,stack,0};
                    op+=1;
                }
                if(n>=d){
                    if(n==d){
                        nArray = get_int(0,input);
                    }
                    else{
                        nArray = ((Object[]) list_tail(get_int(n-d-1,input),symtbl))[0];
                    }
                }
                else {
                    if (op < 3){
                        nArray = ((Object[]) list_tail(n,symtbl))[0];
                    }
                    else{
                        nArray = n;
                    }
                }
                if (4 < op){
                    nArray = new Object[]{new Object[]{nArray,0,pop()},NIL,1};                                   // Array
                    if (stack==NIL || ((Object[])stack)[1]==(Integer)0)break;
                    op = 4;
                }
            }
            ((Object[]) stack)[0] = new Object[]{op - 1, nArray, ((Object[]) stack)[0]};
        }
        Object[] temp = (Object[]) ((Object[]) nArray)[0];
        Object[] pc = (Object[]) temp[2];
        set_global(new Object[]{0, symtbl, 1});
        set_global(FALSE);
        set_global(TRUE);
        set_global(NIL);

        stack = new Object[]{0, 0, new Object[]{5, 0, 0}};
        int j = 0;
        while (true){
            Object o = pc[1];
            Object i = pc[0];
            if  ((int) i < 1){
                Object temp2 = ((Object[]) get_opnd(o))[0];
                while (true){
                    Object temp3;
                    if (temp2 instanceof Object[]){
                        temp3 = ((Object[])temp2)[0];
                    }
                    else{
                        temp3 = temp2;
                    }
                    if (is_rib(temp3)){
                        Object[] c = (Object[]) temp3;
                        Object[] c2 = new Object[]{0, temp2, 0};
                        Object[] s2 = c2;
                        int nparams = ((int) c[0]) >> 1;
                        while (nparams > 0){
                            s2 = new Object[]{pop(), s2, 0};
                            nparams -= 1;
                        }
                        if (pc[2] instanceof Object[]){
                            c2[0] = stack;
                            c2[2] = pc[2];
                        }
                        else {
                            Object[] k = get_cont();
                            c2[0] = k[0];
                            c2[2] = k[2];
                        }
                        stack = s2;
                        pc = c;
                        break;
                    }
                    else {
                        Object[] c;
                        int cInt = (int)temp3;
                        prim(cInt);
                        if (pc[2] instanceof Object[]){
                            c = pc;
                        }
                        else{
                            c = get_cont();
                            ((Object[])stack)[1] = c[0];
                        }
                        pc=c;
                        break;
                    }

                }
            }
            else if((int)i < 2){
                ((Object[])get_opnd(o))[0] = ((Object[])stack)[0];
                stack = ((Object[])stack)[1];
            }
            else if ((int)i < 3){
                push(((Object[])get_opnd(o))[0]);
            }
            else if ((int)i < 4){
                push(o);
            }
            else if ((int)i < 5){
                Object tempPop = pop();
                if(!(tempPop==FALSE)){
                    pc = (Object[]) pc[1];
                    continue;
                }
            }
            else{
                break;
            }
            pc = (Object[]) pc[2];
        }
    }
    private static Object get_opnd(Object o){
        return is_rib(o) ? o :  list_tail((int) o, ((Object[])stack));
    }

    public static Object[] get_cont(){
        Object[] s = ((Object[])stack);
        while (!(s[2] instanceof Object[])){
            s = (Object[]) s[1];
        }
        return s;
    }
    public static void set_global(Object val){
        Object[] temp = (Object[]) symtbl[0];
        temp[0] = val;
        symtbl =  (Object[]) symtbl[1];
    }
    private static void prim1(){
        x = pop();
    }
    private static void prim2(){
        y = pop();
        prim1();
    }
    private static void prim3(){
        z = pop();
        prim2();
    }
    public static void prim(int no){
        switch (no){
            case 0 -> {prim3();push(new Object[]{x,y,z});break;}
            case 1 -> {prim1();push(x);break;}
            case 2 -> {pop();break;}
            case 3 -> {x = pop();pop();push(x);break;}
            case 4 -> {Object[] temp = (Object[]) pop();push(new Object[]{temp[0],stack,1});break;}
            case 5 -> {prim1();push(bool2scm(is_rib(x)));break;}
            case 6 -> {prim1();push(((Object[]) x)[0]);break;}
            case 7 -> {prim1();push(((Object[]) x)[1]);break;}
            case 8 -> {prim1();push(((Object[]) x)[2]);break;}
            case 9 -> {prim2();push(f0s(y,(Object[]) x));break;}
            case 10 -> {prim2();push(f1s(y,(Object[]) x));break;}
            case 11 -> {prim2();push(f2s(y,(Object[]) x));break;}
            case 12 -> {prim2();
                push(bool2scm(x == y));break;}
            case 13 -> {prim2();push(bool2scm((int)x < (int)y));break;}
            case 14 -> {prim2();push((int)x + (int)y);break;}
            case 15 -> {prim2();push((int)x - (int)y);break;}
            case 16 -> {prim2();push((int)x * (int) y);break;}
            case 17 -> {prim2();push((int)x /(int)y);break;}
            case 18 -> {getChar();break;}
            case 19 -> {
                x = ((Object[]) stack)[0];
                putChar((char)(int)x);
                break;}
            case 20 -> {prim1();System.exit((int)x);}
        }
    }
    private static Object printStack(Object stack){
        if (!(stack instanceof Object[])){
            return stack;
        }
        Object x = ((Object[]) stack)[0];
        Object y = ((Object[]) stack)[1];
        Object z =((Object[]) stack)[2];
        return "["+printStack(x)+","+printStack(y)+","+printStack(z)+"]";
    }
    //function that retrieve the byte from input string
    public static int get_byte(String input) {
        pos++;
        return input.charAt(pos);
    }

    public static int get_Code(String input) {
        int byte_value = get_byte(input) - 35;
        if (byte_value < 0) {
            return 57;
        }
        return byte_value;
    }

    public static int get_int(int accumulator, String input) {
        int nextByte = get_Code(input);
        accumulator *= 46;
        accumulator += nextByte;
        if (nextByte < 46) {
            return accumulator;
        }
        return get_int(accumulator - 46, input);
    }

    public static void push(Object value) {
        stack = new Object[]{value,stack,0};
    }

    public static Object pop() {
        Object x = ((Object[])stack)[0];
        Object value = ((Object[])stack)[1];
        stack = value;
        return x;
    }

    public static char putChar(char c) {
        System.out.print(c);
        return c;
    }

    public static void getChar() {
        try {
            int c = System.in.read();
            if (c == -1) {
                push(-1);
            }
            push(c);
        } catch (IOException e) {
            System.out.println("IOexception");
        }
    }
    public static Object f0s(Object y,Object[] x){x[0] = y;return y;}
    public static Object f1s(Object y,Object[] x){x[1] = y;return y;}
    public static Object f2s(Object y,Object[] x){x[2] = y;return y;}

    public static Object bool2scm(boolean x){
        return x ? TRUE : FALSE;
    }
    public static boolean is_rib(Object x){
        return x instanceof Object[];
    }
    public static Object[] scm2list(Object[] l){
        return !Arrays.equals(l, NIL) ? append(new Object[]{l[0]},scm2list(reduceArray(l))) : new Object[]{};
    }
    public static Object[] list_str2scm(String l){
        return !l.isEmpty() ? new Object[]{l.charAt(0),list_str2scm(l.substring(1)),0} : NIL;
    }
    public static String scm2str(Object[] s){
        return chars2str((Object[])s[0]);
    }
    public static String chars2str(Object[] c){
        return Arrays.equals(c,NIL) ? c[0]+chars2str(reduceArray(c)) : "";
    }
    public static Object[] str2scm(String s){
        return new Object[]{char2scm(s),s.length(),3};
    }
    public static Object[] char2scm(String c){
        return !c.isEmpty() ? new Object[]{c.charAt(0),str2scm(c.substring(1)),0} : NIL;
    }
    private static Object[] reduceArray(Object[] array){
        Object[] temp = new Object[array.length-1];
        for(int i = array.length-1; i>1;--i){
            temp[i-1] = array[i];
        }
        return temp;
    }
    private static Object[] append(Object[] a,Object[] b){
        Object[] temp = new Object[a.length+b.length];
        int index = 0;
        for(Object c : a){
            temp[index++] = c;
        }
        for(Object d : b){
            temp[index++] = d;
        }
        return temp;
    }
    private static Object list_tail(int i, Object list){
        if (i==0){
            return list;
        }
        return list_tail(i-1, ((Object[]) list)[1]);
    }
    public static String arrayToString(Object obj) {
        if (obj instanceof Object[]) {
            Object[] arr = (Object[]) obj;
            StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < arr.length; i++) {
                sb.append(arrayToString(arr[i])); // Recursively process nested arrays
                if (i < arr.length - 1) {
                    sb.append(", ");
                }
            }
            sb.append("]");
            return sb.toString();
        } else {
            return String.valueOf(obj); // Convert non-array objects to strings
        }
    }

}


