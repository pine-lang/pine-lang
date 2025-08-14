INSERT INTO x.company (id) VALUES (1), (2);

-- Employees for company 1
INSERT INTO y.employee (id, company_id, reports_to) VALUES
  (10, 1, NULL),   -- CEO
  (11, 1, 10),     -- Manager reports to 10
  (12, 1, 11);     -- Engineer reports to 11

-- Employees for company 2
INSERT INTO y.employee (id, company_id, reports_to) VALUES
  (20, 2, NULL),
  (21, 2, 20);

-- Documents related to employees and companies
INSERT INTO z.document (id, employee_id, created_by, company_id) VALUES
  (100, 12, 11, 1),
  (101, 11, 10, 1),
  (102, 21, 20, 2); 