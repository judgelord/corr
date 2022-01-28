SELECT cycle, recipient_icpsr, recipient_type, recipient_name, recipient_icpsr, contributor_ext_id, bonica_rid, bonica_id2, contributor_category FROM newconts WHERE  recipient_type = 'CAND' AND seat IN ('federal:house', 'federal:senate') GROUP BY cycle, contributor_ext_id, recipient_icpsr ;


 SELECT cycle, recipient_icpsr, recipient_type, recipient_name, recipient_icpsr, contributor_ext_id, bonica_rid, bonica_id2, contributor_category, sum(amount) AS sum_amount FROM newconts WHERE  recipient_type = 'CAND' AND seat IN ('federal:house', 'federal:senate') GROUP BY cycle, contributor_ext_id, recipient_icpsr INTO OUTFILE '/home/groups/andyhall/np_dime/GrimQuery.csv' FIELDS TERMINATED BY ',' ENCLOSED BY '"' LINES TERMINATED BY '\n';



SELECT cycle, recipient_type, recipient_name, recipient_icpsr, bonica_cid, contributor_ext_id,contributor_type, contributor_name, contributor_city, contributor_address, contributor_employer, contributor_occupation, contributor_ext_id, bonica_rid, bonica_id2, contributor_category, sum(amount) AS sum_amount FROM newconts WHERE  recipient_type = 'CAND' AND seat IN ('federal:house', 'federal:senate') GROUP BY cycle, bonica_cid, bonica_rid INTO OUTFILE '/home/groups/andyhall/np_dime/GrimQuery2.csv' FIELDS TERMINATED BY ',' ENCLOSED BY '"' LINES TERMINATED BY '\n';
