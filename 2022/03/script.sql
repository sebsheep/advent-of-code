BEGIN;

create or replace function to_priority(text) returns integer as $$
begin
    return case $1 
            WHEN 'a' THEN 1
            WHEN 'b' THEN 2
            WHEN 'c' THEN 3
            WHEN 'd' THEN 4
            WHEN 'e' THEN 5
            WHEN 'f' THEN 6
            WHEN 'g' THEN 7
            WHEN 'h' THEN 8
            WHEN 'i' THEN 9
            WHEN 'j' THEN 10
            WHEN 'k' THEN 11
            WHEN 'l' THEN 12
            WHEN 'm' THEN 13
            WHEN 'n' THEN 14
            WHEN 'o' THEN 15
            WHEN 'p' THEN 16
            WHEN 'q' THEN 17
            WHEN 'r' THEN 18
            WHEN 's' THEN 19
            WHEN 't' THEN 20
            WHEN 'u' THEN 21
            WHEN 'v' THEN 22
            WHEN 'w' THEN 23
            WHEN 'x' THEN 24
            WHEN 'y' THEN 25
            WHEN 'z' THEN 26
            WHEN 'A' THEN 27
            WHEN 'B' THEN 28
            WHEN 'C' THEN 29
            WHEN 'D' THEN 30
            WHEN 'E' THEN 31
            WHEN 'F' THEN 32
            WHEN 'G' THEN 33
            WHEN 'H' THEN 34
            WHEN 'I' THEN 35
            WHEN 'J' THEN 36
            WHEN 'K' THEN 37
            WHEN 'L' THEN 38
            WHEN 'M' THEN 39
            WHEN 'N' THEN 40
            WHEN 'O' THEN 41
            WHEN 'P' THEN 42
            WHEN 'Q' THEN 43
            WHEN 'R' THEN 44
            WHEN 'S' THEN 45
            WHEN 'T' THEN 46
            WHEN 'U' THEN 47
            WHEN 'V' THEN 48
            WHEN 'W' THEN 49
            WHEN 'X' THEN 50
            WHEN 'Y' THEN 51
            WHEN 'Z' THEN 52
    end;
end;
$$ language plpgsql immutable;

CREATE TABLE input
    ( id SERIAL
    , input TEXT
    );

COPY input(input)
FROM '/home/sebsheep/progs_div/advent-of-code/2022/03/test.csv'
DELIMITER ','
;

-- PART 1
CREATE VIEW  left_rucksack AS
SELECT 
    id
  , regexp_split_to_table(substring(input from char_length(input)/2+1 for char_length(input)/2), '')
     AS priority  
FROM input;

CREATE VIEW right_rucksack AS
SELECT
    id
  , regexp_split_to_table(substring(input from 1 for char_length(input)/2), '')
    AS priority  
FROM input;


CREATE VIEW common_priority AS
SELECT DISTINCT left_rucksack.id, left_rucksack.priority
FROM left_rucksack, right_rucksack
WHERE left_rucksack.id = right_rucksack.id 
AND left_rucksack.priority = right_rucksack.priority;

SELECT SUM(to_priority(priority)) AS part_1 FROM common_priority;

-- PART 2
CREATE VIEW priorities_by_group AS
SELECT (id-1)/3 as grp_id, priority
FROM
    (
        SELECT DISTINCT * FROM left_rucksack
        UNION 
        SELECT DISTINCT * FROM right_rucksack
    ) t
;

SELECT SUM(v) 
FROM ( SELECT to_priority(priority)
FROM priorities_by_group
GROUP BY grp_id, priority
HAVING COUNT(priority) = 3) AS t(v)
;

ROLLBACK;