BEGIN;

create or replace function nullable_cast_to_int(text) returns integer as $$
begin
    return cast($1 as integer);
exception
    when invalid_text_representation then
        return NULL;
end;
$$ language plpgsql immutable;

CREATE TABLE calories_input
    ( id SERIAL
    , calories TEXT
    );

COPY calories_input(calories)
FROM '/home/sebsheep/progs_div/advent-of-code/2022/01/input.csv'
DELIMITER ','
;


CREATE VIEW calories AS 
SELECT id
     , nullable_cast_to_int(calories) AS calories
FROM calories_input     
     ;

CREATE VIEW calories_grouped AS
SELECT 
      calories     
     , id - SUM(
        CASE
        WHEN calories IS NULL THEN 0
        ELSE 1
        END
      ) OVER (  
        ORDER BY id
        ROWS BETWEEN UNBOUNDED PRECEDING
            AND     0         PRECEDING
      ) AS grp
FROM calories;

CREATE VIEW summed_groups AS
SELECT grp, SUM(calories) as summed_calories
FROM calories_grouped
GROUP BY grp;

SELECT MAX(summed_calories) AS part1 FROM summed_groups;


SELECT SUM(v) AS part2 FROM (SELECT summed_calories FROM summed_groups
ORDER BY summed_calories DESC
LIMIT 3)
 as t(v);



ROLLBACK;