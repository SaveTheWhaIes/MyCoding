import java.io.*;
import java.util.*;

public class TownMapApp {

    // weighted graph
    static class Edge {
        String target;
        double weight;

        public Edge(String target, double weight) {
            this.target = target;
            this.weight = weight;
        }
    }

    static class WeightedDigraph {
        private Map<String, List<Edge>> adj;

        public WeightedDigraph() {this.adj = new HashMap<>();}

        public void addVertex(String v) {adj.putIfAbsent(v, new ArrayList<>());}

        public void addEdge(String source, String target, double weight) {
            addVertex(source);
            addVertex(target);
            adj.get(source).add(new Edge(target, weight));
        }

        public Set<String> getVertices() {return adj.keySet();}

        public List<Edge> getEdges(String v) {return adj.get(v);}

        public void loadFromFile(String filename) {
            try (Scanner scanner = new Scanner(new File(filename))) {
                while (scanner.hasNextLine()) {
                    String line = scanner.nextLine().trim();
                    if (line.isEmpty()) continue;

                    String[] parts = line.split("[,\\s]+");

                    if (parts.length >= 3) {
                        try {
                            String source = parts[0];
                            String target = parts[1];
                            double weight = Double.parseDouble(parts[2]);
                            addEdge(source, target, weight);
                        } catch (NumberFormatException e) {
                            System.out.println("Skipping invalid line: " + line);
                        }
                    }
                }
                System.out.println("Map loaded from " + filename);
            } catch (FileNotFoundException e) {
                System.out.println("ERROR: File " + filename + " not found.");
            }
        }
    }

    // count stats
    public static void printMapStats(WeightedDigraph g) {
        int nodes = g.getVertices().size();
        int edges = 0;
        for (String v : g.getVertices()) {edges += g.getEdges(v).size();}
        System.out.println("Points of Interest: " + nodes);
        System.out.println("One-way Streets: " + edges);
    }

    // Dijkstra's algo
    public static void runDijkstra(WeightedDigraph g, String startNode) {
        if (!g.getVertices().contains(startNode)) {
            System.out.println("Start location '" + startNode + "' is not on the map.");
            return;
        }

        Map<String, Double> distances = new HashMap<>();
        PriorityQueue<NodeDist> pq = new PriorityQueue<>(Comparator.comparingDouble(nd -> nd.dist));
        Set<String> visited = new HashSet<>();

        for (String v : g.getVertices()) {distances.put(v, Double.MAX_VALUE);}
        distances.put(startNode, 0.0);
        pq.add(new NodeDist(startNode, 0.0));

        while (!pq.isEmpty()) {
            NodeDist current = pq.poll();
            String u = current.node;

            if (visited.contains(u)) continue;
            visited.add(u);

            if (g.getEdges(u) != null) {
                for (Edge e : g.getEdges(u)) {
                    if (!visited.contains(e.target)) {
                        double newDist = distances.get(u) + e.weight;
                        if (newDist < distances.get(e.target)) {
                            distances.put(e.target, newDist);
                            pq.add(new NodeDist(e.target, newDist));
                        }
                    }
                }
            }
        }

        System.out.println("\nShortest paths from " + startNode + ":");
        for (String v : distances.keySet()) {
            double d = distances.get(v);
            if (d == Double.MAX_VALUE) {
                System.out.println(" -> " + v + ": Unreachable");
            } else {
                System.out.println(" -> " + v + ": " + (int)d + " km");
            }
        }
    }

    // helper for queue
    static class NodeDist {
        String node;
        double dist;
        public NodeDist(String node, double dist) {
            this.node = node;
            this.dist = dist;
        }
    }

    public static void main(String[] args) {
        WeightedDigraph map = new WeightedDigraph();
        map.loadFromFile("small_town.txt");
        System.out.println("\n--- Map Analysis ---");
        printMapStats(map);
        runDijkstra(map, "Home");
        System.out.println("--------------------");
    }
}
