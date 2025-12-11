with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

procedure Task1 is
        package StringArrays is
                type StringArray is array (Positive range <>) of Unbounded_String;
        end StringArrays;
        use StringArrays;

package MapPackage is new Ada.Containers.Indefinite_Hashed_Maps
                (Key_Type => String,
                Element_Type => StringArray,
                Hash => Ada.Strings.Hash,
                Equivalent_Keys => "=");
        use MapPackage;

        type DSplit is array (Positive range <>) of Unbounded_String;
        maxSplitSize : constant := 100;

        function findAllPaths(
                        graph : Map
                ) return StringArray is

        begin
                -- create array from nodes pointing to "out"
                -- build all possible paths from those nodes
        end findAllPaths;

        procedure printGraph(
                        graph : Map
                ) is

                cursor : MapPackage.Cursor := graph.First;
        begin
                while Has_Element(cursor) loop
                        declare
                                currentKey : constant String := Key(cursor);
                                currentArray : constant StringArray := Element(cursor);
                        begin
                                Put(currentKey & " : ");
                                for index in currentArray'Range loop
                                        Put(To_String(currentArray(index)));
                                        if index /= currentArray'Last then
                                                Put(", ");
                                        end if;
                                end loop;
                                New_Line;
                        end;
                        Next(cursor);
                end loop;
        end printGraph;

        function splitString(
                source : String;
                delim : Ada.Strings.Maps.Character_Set) return StringArray is
                result : DSplit(1 .. maxSplitSize);

                count : Natural := 0;
                currentIndex : Positive := Source'First;
                nextIndex : Natural;
        begin
                if source'Length = 0 then
                        return StringArray'(1 .. 0 => Null_Unbounded_String);
                end if;

                loop
                        nextIndex := Ada.Strings.Fixed.Index(Source => source,
                                                             Set => delim,
                                                             From => currentIndex);
                        declare
                                endIndex : constant Positive :=
                                        (if nextIndex = 0 then source'Last else nextIndex - 1);
                                part : constant String := source(currentIndex .. endIndex);
                                trimmedPart : constant String := Ada.Strings.Fixed.Trim(part, Ada.Strings.Both);
                        begin
                                if trimmedPart'Length > 0 then
                                        count := count + 1;
                                        if count > maxSplitSize then
                                                exit;
                                        end if;
                                        result(count) := To_Unbounded_String(trimmedPart);
                                end if;
                        end;
                        if nextIndex = 0 then
                                exit;
                        else
                                currentIndex := nextIndex + 1;
                        end if;
                end loop;

                declare
                        finalResult : StringArray(1 .. count);
                begin
                        for i in 1 .. count loop
                                finalResult(i) := result(i);
                        end loop;
                        return finalResult;
                end;
        end splitString;

        inputFile : File_Type;
        line : String(1 .. 256);
        last : Natural;
        colonDelim : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set(':');
        spaceDelim : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set(' ');
        graphMap : Map;

begin
        Open(File => inputFile, Mode => In_File, Name => "test");
        readLoop:
        loop
                begin
                        Get_Line(File => inputFile, Item => line, Last => last);
                exception
                        when End_Error => exit readLoop;
                end;

                declare
                        lineParts : constant StringArray := splitString(line(1 .. last), colonDelim);
                begin
                        if lineParts'Length > 1 then
                                declare
                                        key : constant String := To_String(lineParts(1));
                                        valueString : constant String := To_String(lineParts(2));
                                        valueParts : constant StringArray := splitString(valueString, spaceDelim);
                                begin
                                        if valueParts'Length > 0 then
                                                if graphMap.Contains(key) then
                                                        Put_Line("Key already exists: " & key);
                                                else
                                                        graphMap.Insert(key, valueParts);
                                                end if;
                                        end if;
                                end;
                        end if;
                end;
        end loop readLoop;
        Close(inputFile);

        

        printGraph(graphMap);
end Task1;
