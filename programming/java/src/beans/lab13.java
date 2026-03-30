class Student {
    private int id;
    private String name;
    private double gpa;

    public Student() {
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public double getGpa() {
        return gpa;
    }

    public void setGpa(double gpa) {
        this.gpa = gpa;
    }
}

public class lab13 {
    public static void main(String[] args) {
        Student student = new Student();

        student.setId(101);
        student.setName("Sudan");
        student.setGpa(3.85);

        System.out.println("Student ID: " + student.getId());
        System.out.println("Student Name: " + student.getName());
        System.out.println("Student GPA: " + student.getGpa());
    }
}
