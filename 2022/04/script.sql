BEGIN;

DROP TABLE IF EXISTS input;

CREATE TABLE input
    ( id SERIAL
    , input1 TEXT
    , input2 TEXT
    );

COPY input(input1, input2)
FROM '/home/sebsheep/progs_div/advent-of-code/2022/04/input.csv'
DELIMITER ','
;

CREATE VIEW structured_input AS
SELECT id 
     , split_part(input1, '-',1)::INT AS mini
     , split_part(input1, '-',2)::INT AS maxi
     , 1 AS elve
FROM input
UNION ALL
SELECT id 
     , split_part(input2, '-',1)::INT AS mini
     , split_part(input2, '-',2)::INT AS maxi
     , 2 AS elve
FROM input
;


-- PART 1

CREATE VIEW fully_overlaping_ids AS
SELECT DISTINCT  input_1.id
FROM  structured_input AS input_1
JOIN  structured_input AS input_2
    ON input_1.id = input_2.id
    AND input_1.elve <> input_2.elve
WHERE
     input_1.mini <= input_2.mini
     AND input_1.maxi >= input_2.maxi
;

SELECT COUNT(*)
FROM fully_overlaping_ids;

-- PART 2

CREATE VIEW overlaping_ids AS
SELECT input_1.id
FROM  structured_input AS input_1
JOIN  structured_input AS input_2
    ON input_1.id = input_2.id
    AND input_1.elve > input_2.elve
WHERE
     input_1.maxi >= input_2.mini
     AND input_1.mini <= input_2.maxi
;


SELECT COUNT(*)
FROM overlaping_ids;

ROLLBACK;