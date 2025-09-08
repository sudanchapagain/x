package src;

// Generic class, Generic Method, Getter, setter, bounded generics
public class GenericClass<T> {
    // To bound to only certain types `GenericClass<T extends Number>`
    private T content;

    public GenericClass(T content) {
        this.content = content;
    }

    // Getter
    public T getContent() {
        return content;
    }

    // Setter
    public void setContent(T content) {
        this.content = content;
    }

    // Generic method
    public <T> void printArray(T[] array) {
        for (T element : array) {
            System.out.println(element);
        }
    }

    // NOTE:
    // Interfaces can be made with generics.
    // When to use Type parameter (T) or Wildcard (?)
    // use (T)
    //      When type is known,
    //      will be known at instantiation or invocation,
    //      Work with specific type,
    //      Ensure type safety.
    // use (?)
    //      When type is not known,
    //      do not care about type,
    //      only read from a collection.
    // use (? extends T)
    //      only read from a collection,
    //      ensure its of type T or a subtype,
    //      no need to add items to collection.
    // use (? super T)
    //      add items of type T to a collection,
    //      allow any type that is a supertype of T,
    //      only need to read items as Object,
    //      ensure that the collection can accept T or its subtypes.
}
