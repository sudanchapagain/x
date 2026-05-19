package np.com.sudanchapagain.gol;

import java.util.Scanner;

public class Main {
    private static final int WIDTH = 10;
    private static final int HEIGHT = 10;
    private static final double SLEEP = 0.5;

    public static void clear() {
        // don't really like this but will have to make do
        System.out.print("\033[H\033[2J");
        System.out.flush();
    }

    public static boolean[][] emptyGrid() {
        return new boolean[HEIGHT][WIDTH];
    }

    public static void printGrid(boolean[][] grid) {
        System.out.print("   ");
        for (int x = 0; x < WIDTH; x++) {
            System.out.print(x + " ");
        }
        System.out.println();

        System.out.print("  ╭");
        for (int i = 0; i < 2 * WIDTH; i++) System.out.print("─");
        System.out.println("╮");

        for (int y = 0; y < HEIGHT; y++) {
            System.out.printf("%2d│", y);
            for (int x = 0; x < WIDTH; x++) {
                System.out.print(grid[y][x] ? "* " : "  ");
            }
            System.out.println("│");
        }

        System.out.print("  ╰");
        for (int i = 0; i < 2 * WIDTH; i++) System.out.print("─");
        System.out.println("╯");
    }

    public static int countNeighbors(boolean[][] grid, int x, int y) {
        int[][] directions = {{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}};
        int count = 0;
        for (int[] d : directions) {
            int nx = x + d[0];
            int ny = y + d[1];
            if (nx >= 0 && nx < WIDTH && ny >= 0 && ny < HEIGHT) {
                if (grid[ny][nx]) count++;
            }
        }
        return count;
    }

    public static boolean applyRules(boolean alive, int neighbors) {
        return (neighbors == 3) || (alive && neighbors == 2);
    }

    public static boolean[][] nextGeneration(boolean[][] grid) {
        boolean[][] newGrid = new boolean[HEIGHT][WIDTH];
        for (int y = 0; y < HEIGHT; y++) {
            for (int x = 0; x < WIDTH; x++) {
                newGrid[y][x] = applyRules(grid[y][x], countNeighbors(grid, x, y));
            }
        }
        return newGrid;
    }

    public static void inputCells(boolean[][] grid, Scanner sc) {
        System.out.println("\nEnter live cell coordinates. Type 'done' when finished.");
        while (true) {
            System.out.print("Cell (x y): ");
            if (!sc.hasNextLine()) {
                System.out.println("\nNo more input. Exiting setup.");
                break;
            }

            String entry = sc.nextLine().trim();
            if (entry.isEmpty()) {
                System.out.println("Please enter coordinates or 'done'.");
                continue;
            }

            if (entry.equalsIgnoreCase("done")) break;

            try {
                String[] parts = entry.split("\\s+");
                if (parts.length != 2) {
                    System.out.println("Enter two numbers: x y");
                    continue;
                }

                int x = Integer.parseInt(parts[0]);
                int y = Integer.parseInt(parts[1]);

                if (x >= 0 && x < WIDTH && y >= 0 && y < HEIGHT) {
                    grid[y][x] = !grid[y][x];
                    clear();
                    printGrid(grid);
                } else {
                    System.out.println("Out of bounds. Valid range: x 0–" + (WIDTH - 1) + ", y 0–" + (HEIGHT - 1));
                }
            } catch (NumberFormatException e) {
                System.out.println("Invalid input. Enter two numbers: x y");
            }
        }
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        boolean[][] grid = emptyGrid();

        clear();
        printGrid(grid);
        inputCells(grid, sc);

        label:
        while (true) {
            System.out.print("\nMode? [s]tep / [r]un / [q]uit: ");
            if (!sc.hasNextLine()) break;
            String mode = sc.nextLine().trim().toLowerCase();
            if (mode.isEmpty()) continue;

            switch (mode) {
                case "q":
                    break label;
                case "s":
                    grid = nextGeneration(grid);
                    clear();
                    printGrid(grid);
                    break;
                case "r":
                    System.out.println("Press Enter to stop running and return to menu.");
                    boolean running = true;
                    while (running) {
                        grid = nextGeneration(grid);
                        clear();
                        printGrid(grid);

                        long endTime = System.currentTimeMillis() + (long) (SLEEP * 1000);
                        while (System.currentTimeMillis() < endTime) {
                            try {
                                if (System.in.available() > 0) {
                                    running = false;
                                    break;
                                }
                            } catch (Exception ignored) {
                            }
                        }
                    }
                    break;
                default:
                    System.out.println("Invalid input. Choose [s], [r], or [q].");
                    break;
            }
        }
        sc.close();
    }
}
