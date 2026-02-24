package src;

import java.util.*;

// collections framework: no use of collections like list, set, map, or queue.
public class Collect {
    public void collect() {
        Set<String> myset = new HashSet<>();
        myset.add("Hello");
        System.out.println(myset);

        List<String> list = new ArrayList<>();
        list.add("Apple");
        list.add("Banana");
        list.add("Cherry");

        List<String> linkedList = new LinkedList<>();
        linkedList.add("Apple");
        linkedList.add("Banana");
        linkedList.add("Cherry");

        Set<String> set = new LinkedHashSet<>();
        set.add("Apple");
        set.add("Banana");
        set.add("Cherry");

        Queue<String> queue1 = new PriorityQueue<>();
        queue1.add("Banana");
        queue1.add("Apple");
        queue1.add("Cherry");

        Queue<String> queue = new LinkedList<>();
        queue.add("Apple");
        queue.add("Banana");
        queue.add("Cherry");

        Map<String, Integer> map = new HashMap<>();
        map.put("Apple", 1);
        map.put("Banana", 2);
        map.put("Cherry", 3);

        Map<String, Integer> map1 = new TreeMap<>();
        map1.put("Banana", 2);
        map1.put("Apple", 1);
        map1.put("Cherry", 3);

        Map<String, Integer> map2 = new LinkedHashMap<>();
        map2.put("Apple", 1);
        map2.put("Banana", 2);
        map2.put("Cherry", 3);

        Iterator<String> iterator = list.iterator();
        while (iterator.hasNext()) {
            String item = iterator.next();
            System.out.println(item);
        }

        Collections.sort(list, new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return o2.compareTo(o1);
            }
        });
    }
}
