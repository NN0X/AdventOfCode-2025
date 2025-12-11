with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Interfaces; use Interfaces;

procedure Task2 is
        package StringVectors is new Ada.Containers.Vectors
                (Index_Type => Positive,
                 Element_Type => Unbounded_String);
        use StringVectors;

        package GMap is new Ada.Containers.Indefinite_Hashed_Maps
                (Key_Type => String,
                 Element_Type => StringVectors.Vector,
                 Hash => Ada.Strings.Hash,
                 Equivalent_Keys => "=");
        use GMap;

        package PathCount is new Ada.Containers.Indefinite_Hashed_Maps
                (Key_Type => String,
                 Element_Type => Natural,
                 Hash => Ada.Strings.Hash,
                 Equivalent_Keys => "=");
        use PathCount;

        package InDegree is new Ada.Containers.Indefinite_Hashed_Maps
                (Key_Type => String,
                 Element_Type => Natural,
                 Hash => Ada.Strings.Hash,
                 Equivalent_Keys => "=");
        use InDegree;

        package StringQueue is new Ada.Containers.Vectors
                (Index_Type => Positive,
                 Element_Type => Unbounded_String);

        type StateType is range 0 .. 3;

        type StateKey is record
                node : Unbounded_String;
                state : StateType;
        end record;

        function stateHash(key : StateKey) return Ada.Containers.Hash_Type is
                use Ada.Containers;
                nodeHash : constant Hash_Type := Ada.Strings.Hash(To_String(key.node));
                stateVal : constant Unsigned_32 := Unsigned_32(key.state);
                shifted : constant Unsigned_32 := Shift_Left(stateVal, 10);
        begin
                return nodeHash xor Hash_Type(shifted);
        end stateHash;

        function stateEqual(left, right : StateKey) return Boolean is
        begin
                return left.node = right.node and left.state = right.state;
        end stateEqual;

        package StateCounts is new Ada.Containers.Indefinite_Hashed_Maps
                (Key_Type => StateKey,
                 Element_Type => Long_Long_Integer,
                 Hash => stateHash,
                 Equivalent_Keys => stateEqual);

        procedure calculateInDegrees(
                graph : GMap.Map;
                allNodes : StringVectors.Vector;
                inDegreeMap : in out InDegree.Map
        ) is
                c : GMap.Cursor := graph.First;
        begin
                for n of allNodes loop
                        inDegreeMap.Insert(To_String(n), 0);
                end loop;
                while Has_Element(c) loop
                        declare
                                neighbors : constant StringVectors.Vector := Element(c);
                        begin
                                for nb of neighbors loop
                                        declare
                                                s : constant String := To_String(nb);
                                                d : constant Natural := inDegreeMap.Element(s);
                                        begin
                                                inDegreeMap.Replace(s, d + 1);
                                        end;
                                end loop;
                        end;
                        Next(c);
                end loop;
        end calculateInDegrees;

        procedure initializeQueue(
                inDegreeMap : InDegree.Map;
                queue : in out StringQueue.Vector
        ) is
                c : InDegree.Cursor := inDegreeMap.First;
        begin
                while Has_Element(c) loop
                        if Element(c) = 0 then
                                queue.Append(To_Unbounded_String(Key(c)));
                        end if;
                        Next(c);
                end loop;
        end initializeQueue;

        procedure processNodeNeighbors(
                graph : GMap.Map;
                nodeStr : String;
                inDegreeMap : in out InDegree.Map;
                queue : in out StringQueue.Vector
        ) is
        begin
                if graph.Contains(nodeStr) then
                        declare
                                neighbors : constant StringVectors.Vector := graph.Element(nodeStr);
                        begin
                                for nb of neighbors loop
                                        declare
                                                s : constant String := To_String(nb);
                                                d : Natural := inDegreeMap.Element(s);
                                        begin
                                                d := d - 1;
                                                inDegreeMap.Replace(s, d);
                                                if d = 0 then
                                                        queue.Append(nb);
                                                end if;
                                        end;
                                end loop;
                        end;
                end if;
        end processNodeNeighbors;

        function topologicalSort(
                graph : GMap.Map;
                allNodes : StringVectors.Vector
        ) return StringVectors.Vector is
                degrees : InDegree.Map;
                queue : StringQueue.Vector;
                result : StringVectors.Vector;
        begin
                calculateInDegrees(graph, allNodes, degrees);
                initializeQueue(degrees, queue);
                while not queue.Is_Empty loop
                        declare
                                node : constant Unbounded_String := queue.First_Element;
                                s : constant String := To_String(node);
                        begin
                                queue.Delete_First;
                                result.Append(node);
                                processNodeNeighbors(graph, s, degrees, queue);
                        end;
                end loop;
                return result;
        end topologicalSort;

        function getInitialState(
                nodeStr : String;
                nodeA : String;
                nodeB : String
        ) return StateType is
        begin
                if nodeStr = nodeA and nodeStr = nodeB then
                        return 3;
                elsif nodeStr = nodeA then
                        return 1;
                elsif nodeStr = nodeB then
                        return 2;
                else
                        return 0;
                end if;
        end getInitialState;

        function updateState(
                currentState : StateType;
                neighborStr : String;
                nodeA : String;
                nodeB : String
        ) return StateType is
        begin
                if neighborStr = nodeA then
                        if currentState = 0 then
                                return 1;
                        elsif currentState = 2 then
                                return 3;
                        end if;
                elsif neighborStr = nodeB then
                        if currentState = 0 then
                                return 2;
                        elsif currentState = 1 then
                                return 3;
                        end if;
                end if;
                return currentState;
        end updateState;

        procedure propagateCountToNeighbor(
                neighbor : Unbounded_String;
                newState : StateType;
                currentCount : Long_Long_Integer;
                countsMap : in out StateCounts.Map
        ) is
                k : StateKey;
                c : Long_Long_Integer := 0;
        begin
                k.node := neighbor;
                k.state := newState;
                if countsMap.Contains(k) then
                        c := countsMap.Element(k);
                end if;
                c := c + currentCount;
                if countsMap.Contains(k) then
                        countsMap.Replace(k, c);
                else
                        countsMap.Insert(k, c);
                end if;
        end propagateCountToNeighbor;

        procedure processNodeState(
                node : Unbounded_String;
                currentState : StateType;
                graph : GMap.Map;
                nodeA : String;
                nodeB : String;
                countsMap : in out StateCounts.Map
        ) is
                k : StateKey;
                c : Long_Long_Integer;
                s : constant String := To_String(node);
        begin
                k.node := node;
                k.state := currentState;
                if not countsMap.Contains(k) then
                        return;
                end if;
                c := countsMap.Element(k);
                if c = 0 or else not graph.Contains(s) then
                        return;
                end if;
                declare
                        neighbors : constant StringVectors.Vector := graph.Element(s);
                begin
                        for nb of neighbors loop
                                declare
                                        ns : constant String := To_String(nb);
                                        newState : constant StateType := updateState(currentState, ns, nodeA, nodeB);
                                begin
                                        propagateCountToNeighbor(nb, newState, c, countsMap);
                                end;
                        end loop;
                end;
        end processNodeState;

        function countPathsThroughBoth(
                graph : GMap.Map;
                source : String;
                target : String;
                nodeA : String;
                nodeB : String;
                allNodes : StringVectors.Vector
        ) return Long_Long_Integer is
                counts : StateCounts.Map;
                topo : constant StringVectors.Vector := topologicalSort(graph, allNodes);
                srcKey : StateKey;
                tgtKey : StateKey;
        begin
                srcKey.node := To_Unbounded_String(source);
                srcKey.state := getInitialState(source, nodeA, nodeB);
                counts.Insert(srcKey, 1);
                for n of topo loop
                        for st in StateType'Range loop
                                processNodeState(n, st, graph, nodeA, nodeB, counts);
                        end loop;
                end loop;
                tgtKey.node := To_Unbounded_String(target);
                tgtKey.state := 3;
                if counts.Contains(tgtKey) then
                        return counts.Element(tgtKey);
                else
                        return 0;
                end if;
        end countPathsThroughBoth;

        function reverseGraph(
                graph : GMap.Map
        ) return GMap.Map is
                reversed : GMap.Map;
                c : GMap.Cursor := graph.First;
        begin
                while Has_Element(c) loop
                        declare
                                fromStr : constant String := Key(c);
                                neighbors : constant StringVectors.Vector := Element(c);
                        begin
                                for nb of neighbors loop
                                        declare
                                                toStr : constant String := To_String(nb);
                                                v : StringVectors.Vector;
                                        begin
                                                if reversed.Contains(toStr) then
                                                        v := reversed.Element(toStr);
                                                else
                                                        v := StringVectors.Empty_Vector;
                                                end if;
                                                v.Append(To_Unbounded_String(fromStr));
                                                if reversed.Contains(toStr) then
                                                        reversed.Replace(toStr, v);
                                                else
                                                        reversed.Insert(toStr, v);
                                                end if;
                                        end;
                                end loop;
                        end;
                        Next(c);
                end loop;
                return reversed;
        end reverseGraph;

        function splitString(
                source : String;
                delim : Ada.Strings.Maps.Character_Set
        ) return StringVectors.Vector is
                result : StringVectors.Vector;
                i : Positive := source'First;
                j : Natural;
        begin
                if source'Length = 0 then
                        return result;
                end if;
                loop
                        j := Ada.Strings.Fixed.Index(Source => source, Set => delim, From => i);
                        declare
                                e : constant Positive := (if j = 0 then source'Last else j - 1);
                                part : constant String := source(i .. e);
                                t : constant String := Ada.Strings.Fixed.Trim(part, Ada.Strings.Both);
                        begin
                                if t'Length > 0 then
                                        result.Append(To_Unbounded_String(t));
                                end if;
                        end;
                        exit when j = 0;
                        i := j + 1;
                end loop;
                return result;
        end splitString;

        procedure collectAllNodes(
                graph : GMap.Map;
                allNodes : in out StringVectors.Vector
        ) is
                set : PathCount.Map;
                c : GMap.Cursor := graph.First;
        begin
                while Has_Element(c) loop
                        declare
                                s : constant String := Key(c);
                        begin
                                if not set.Contains(s) then
                                        set.Insert(s, 0);
                                        allNodes.Append(To_Unbounded_String(s));
                                end if;
                                declare
                                        neighbors : constant StringVectors.Vector := Element(c);
                                begin
                                        for nb of neighbors loop
                                                declare
                                                        ns : constant String := To_String(nb);
                                                begin
                                                        if not set.Contains(ns) then
                                                                set.Insert(ns, 0);
                                                                allNodes.Append(nb);
                                                        end if;
                                                end;
                                        end loop;
                                end;
                        end;
                        Next(c);
                end loop;
        end collectAllNodes;

        procedure parseInputLine(
                line : String;
                last : Natural;
                colonDelim : Ada.Strings.Maps.Character_Set;
                spaceDelim : Ada.Strings.Maps.Character_Set;
                graphMap : in out GMap.Map
        ) is
                parts : constant StringVectors.Vector := splitString(line(1 .. last), colonDelim);
        begin
                if Natural(parts.Length) > 1 then
                        declare
                                k : constant String := To_String(parts.Element(1));
                                vstr : constant String := To_String(parts.Element(2));
                                vals : constant StringVectors.Vector := splitString(vstr, spaceDelim);
                        begin
                                if not vals.Is_Empty and not graphMap.Contains(k) then
                                        graphMap.Insert(k, vals);
                                end if;
                        end;
                end if;
        end parseInputLine;

        inputFile : File_Type;
        line : String(1 .. 256);
        last : Natural;
        colonDelim : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set(':');
        spaceDelim : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set(' ');
        graphMap : GMap.Map;
        allNodes : StringVectors.Vector;
        count : Long_Long_Integer;

begin
        Open(File => inputFile, Mode => In_File, Name => "../inputs/day11");
        loop
                begin
                        Get_Line(File => inputFile, Item => line, Last => last);
                        parseInputLine(line, last, colonDelim, spaceDelim, graphMap);
                exception
                        when End_Error => exit;
                end;
        end loop;
        Close(inputFile);

        collectAllNodes(graphMap, allNodes);

        count := countPathsThroughBoth(graphMap, "svr", "out", "dac", "fft", allNodes);
        Put_Line(Long_Long_Integer'Image(count));
end Task2;
