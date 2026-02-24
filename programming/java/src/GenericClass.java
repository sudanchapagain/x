package src;

// generic class, generic method, getter, setter, bounded generics
public class GenericClass<T> {
    // to bound to only certain types `GenericClass<T extends Number>`
    private T content;

    public GenericClass(T content) {
        this.content = content;
    }

    // getter
    public T getContent() {
        return content;
    }

    // setter
    public void setContent(T content) {
        this.content = content;
    }

    // generic method
    public <T> void printArray(T[] array) {
        for (T element : array) {
            System.out.println(element);
        }
    }

    // NOTE:
    // interfaces can be made with generics.
    // when to use type parameter (T) or wildcard (?)
    // use (T)
    //      when type is known,
    //      will be known at instantiation or invocation,
    //      work with specific type,
    //      ensure type safety.
    // use (?)
    //      when type is not known,
    //      do not care about type,
    //      only read from a collection.
    // use (? extends T)
    //      only read from a collection,
    //      ensure its of type T or a subtype,
    //      no need to add items to collection.
    // use (? super T)
    //      add items of type T to a collection,
    //      allow any type that is a supertype of T,
    //      only need to read items as object,
    //      ensure that the collection can accept T or its subtypes.
}
