with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

procedure Task1 is
        package String_Vectors is new Ada.Containers.Vectors
                (Index_Type => Positive,
                 Element_Type => Unbounded_String);
        use String_Vectors;

        package Graph_Map is new Ada.Containers.Indefinite_Hashed_Maps
                (Key_Type => String,
                 Element_Type => String_Vectors.Vector,
                 Hash => Ada.Strings.Hash,
                 Equivalent_Keys => "=");
        use Graph_Map;

        package Path_Count_Map is new Ada.Containers.Indefinite_Hashed_Maps
                (Key_Type => String,
                 Element_Type => Natural,
                 Hash => Ada.Strings.Hash,
                 Equivalent_Keys => "=");
        use Path_Count_Map;

        package In_Degree_Map is new Ada.Containers.Indefinite_Hashed_Maps
                (Key_Type => String,
                 Element_Type => Natural,
                 Hash => Ada.Strings.Hash,
                 Equivalent_Keys => "=");
        use In_Degree_Map;

        package String_Queue is new Ada.Containers.Vectors
                (Index_Type => Positive,
                 Element_Type => Unbounded_String);

        function topologicalSort(
                graph : Graph_Map.Map;
                allNodes : String_Vectors.Vector
        ) return String_Vectors.Vector is
                inDegree : In_Degree_Map.Map;
                queue : String_Queue.Vector;
                result : String_Vectors.Vector;
        begin
                for node of allNodes loop
                        inDegree.Insert(To_String(node), 0);
                end loop;

                declare
                        cursor : Graph_Map.Cursor := graph.First;
                begin
                        while Has_Element(cursor) loop
                                declare
                                        neighbors : constant String_Vectors.Vector := Element(cursor);
                                begin
                                        for neighbor of neighbors loop
                                                declare
                                                        neighborStr : constant String := To_String(neighbor);
                                                        currentDegree : Natural;
                                                begin
                                                        currentDegree := inDegree.Element(neighborStr);
                                                        inDegree.Replace(neighborStr, currentDegree + 1);
                                                end;
                                        end loop;
                                end;
                                Next(cursor);
                        end loop;
                end;

                declare
                        cursor : In_Degree_Map.Cursor := inDegree.First;
                begin
                        while Has_Element(cursor) loop
                                if Element(cursor) = 0 then
                                        queue.Append(To_Unbounded_String(Key(cursor)));
                                end if;
                                Next(cursor);
                        end loop;
                end;

                while not queue.Is_Empty loop
                        declare
                                node : constant Unbounded_String := queue.First_Element;
                                nodeStr : constant String := To_String(node);
                        begin
                                queue.Delete_First;
                                result.Append(node);

                                if graph.Contains(nodeStr) then
                                        declare
                                                neighbors : constant String_Vectors.Vector := graph.Element(nodeStr);
                                        begin
                                                for neighbor of neighbors loop
                                                        declare
                                                                neighborStr : constant String := To_String(neighbor);
                                                                currentDegree : Natural := inDegree.Element(neighborStr);
                                                        begin
                                                                currentDegree := currentDegree - 1;
                                                                inDegree.Replace(neighborStr, currentDegree);
                                                                if currentDegree = 0 then
                                                                        queue.Append(neighbor);
                                                                end if;
                                                        end;
                                                end loop;
                                        end;
                                end if;
                        end;
                end loop;

                return result;
        end topologicalSort;

        function countPathsDP(
                graph : Graph_Map.Map;
                source : String;
                target : String;
                allNodes : String_Vectors.Vector
        ) return Natural is
                pathCounts : Path_Count_Map.Map;
                topoOrder : String_Vectors.Vector;
        begin
                topoOrder := topologicalSort(graph, allNodes);

                for node of allNodes loop
                        pathCounts.Insert(To_String(node), 0);
                end loop;

                if pathCounts.Contains(target) then
                        pathCounts.Replace(target, 1);
                else
                        pathCounts.Insert(target, 1);
                end if;

                for i in reverse 1 .. Natural(topoOrder.Length) loop
                        declare
                                nodeStr : constant String := To_String(topoOrder.Element(i));
                                currentCount : Natural := pathCounts.Element(nodeStr);
                        begin
                                if graph.Contains(nodeStr) then
                                        declare
                                                neighbors : constant String_Vectors.Vector := graph.Element(nodeStr);
                                        begin
                                                for neighbor of neighbors loop
                                                        declare
                                                                neighborStr : constant String := To_String(neighbor);
                                                                neighborCount : Natural;
                                                        begin
                                                                if pathCounts.Contains(neighborStr) then
                                                                        neighborCount := pathCounts.Element(neighborStr);
                                                                        currentCount := currentCount + neighborCount;
                                                                end if;
                                                        end;
                                                end loop;
                                                pathCounts.Replace(nodeStr, currentCount);
                                        end;
                                end if;
                        end;
                end loop;

                if pathCounts.Contains(source) then
                        return pathCounts.Element(source);
                else
                        return 0;
                end if;
        end countPathsDP;

        function reverseGraph(
                graph : Graph_Map.Map
        ) return Graph_Map.Map is
                reversed : Graph_Map.Map;
                cursor : Graph_Map.Cursor := graph.First;
        begin
                while Has_Element(cursor) loop
                        declare
                                fromNode : constant String := Key(cursor);
                                neighbors : constant String_Vectors.Vector := Element(cursor);
                        begin
                                for neighbor of neighbors loop
                                        declare
                                                toNode : constant String := To_String(neighbor);
                                                newNeighbors : String_Vectors.Vector;
                                        begin
                                                if reversed.Contains(toNode) then
                                                        newNeighbors := reversed.Element(toNode);
                                                else
                                                        newNeighbors := String_Vectors.Empty_Vector;
                                                end if;

                                                newNeighbors.Append(To_Unbounded_String(fromNode));

                                                if reversed.Contains(toNode) then
                                                        reversed.Replace(toNode, newNeighbors);
                                                else
                                                        reversed.Insert(toNode, newNeighbors);
                                                end if;
                                        end;
                                end loop;
                        end;
                        Next(cursor);
                end loop;

                return reversed;
        end reverseGraph;

        function splitString(
                source : String;
                delim : Ada.Strings.Maps.Character_Set
        ) return String_Vectors.Vector is
                result : String_Vectors.Vector;
                currentIndex : Positive := Source'First;
                nextIndex : Natural;
        begin
                if source'Length = 0 then
                        return result;
                end if;

                loop
                        nextIndex := Ada.Strings.Fixed.Index(
                                Source => source,
                                Set => delim,
                                From => currentIndex);

                        declare
                                endIndex : constant Positive :=
                                        (if nextIndex = 0 then source'Last else nextIndex - 1);
                                part : constant String := source(currentIndex .. endIndex);
                                trimmedPart : constant String := 
                                        Ada.Strings.Fixed.Trim(part, Ada.Strings.Both);
                        begin
                                if trimmedPart'Length > 0 then
                                        result.Append(To_Unbounded_String(trimmedPart));
                                end if;
                        end;

                        exit when nextIndex = 0;
                        currentIndex := nextIndex + 1;
                end loop;

                return result;
        end splitString;

        inputFile : File_Type;
        line : String(1 .. 256);
        last : Natural;
        colonDelim : constant Ada.Strings.Maps.Character_Set :=
                Ada.Strings.Maps.To_Set(':');
        spaceDelim : constant Ada.Strings.Maps.Character_Set :=
                Ada.Strings.Maps.To_Set(' ');
        graphMap : Graph_Map.Map;
        reversedGraph : Graph_Map.Map;
        allNodes : String_Vectors.Vector;
        pathCount : Natural;

begin
        Open(File => inputFile, Mode => In_File, Name => "../inputs/day11");

        loop
                begin
                        Get_Line(File => inputFile, Item => line, Last => last);
                exception
                        when End_Error => exit;
                end;

                declare
                        lineParts : constant String_Vectors.Vector :=
                                splitString(line(1 .. last), colonDelim);
                begin
                        if Natural(lineParts.Length) > 1 then
                                declare
                                        key : constant String :=
                                                To_String(lineParts.Element(1));
                                        valueString : constant String :=
                                                To_String(lineParts.Element(2));
                                        valueParts : constant String_Vectors.Vector :=
                                                splitString(valueString, spaceDelim);
                                begin
                                        if not valueParts.Is_Empty then
                                                if not graphMap.Contains(key) then
                                                        graphMap.Insert(key, valueParts);
                                                end if;
                                        end if;
                                end;
                        end if;
                end;
        end loop;
        Close(inputFile);

        reversedGraph := reverseGraph(graphMap);

        declare
                nodeSet : Path_Count_Map.Map;
                cursor : Graph_Map.Cursor := reversedGraph.First;
        begin
                while Has_Element(cursor) loop
                        declare
                                nodeStr : constant String := Key(cursor);
                        begin
                                if not nodeSet.Contains(nodeStr) then
                                        nodeSet.Insert(nodeStr, 0);
                                        allNodes.Append(To_Unbounded_String(nodeStr));
                                end if;

                                declare
                                        neighbors : constant String_Vectors.Vector := Element(cursor);
                                begin
                                        for neighbor of neighbors loop
                                                declare
                                                        neighborStr : constant String := To_String(neighbor);
                                                begin
                                                        if not nodeSet.Contains(neighborStr) then
                                                                nodeSet.Insert(neighborStr, 0);
                                                                allNodes.Append(neighbor);
                                                        end if;
                                                end;
                                        end loop;
                                end;
                        end;
                        Next(cursor);
                end loop;
        end;

        pathCount := countPathsDP(reversedGraph, "out", "you", allNodes);
        Put_Line(Natural'Image(pathCount));

end Task1;
