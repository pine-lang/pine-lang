CREATE SCHEMA IF NOT EXISTS x;
CREATE SCHEMA IF NOT EXISTS y;
CREATE SCHEMA IF NOT EXISTS z;

CREATE TABLE IF NOT EXISTS x.company (
  id INTEGER PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS y.employee (
  id INTEGER PRIMARY KEY,
  company_id INTEGER NOT NULL,
  reports_to INTEGER NULL,
  CONSTRAINT fk_employee_company FOREIGN KEY (company_id) REFERENCES x.company(id),
  CONSTRAINT fk_employee_reports_to FOREIGN KEY (reports_to) REFERENCES y.employee(id)
);

CREATE TABLE IF NOT EXISTS z.document (
  id INTEGER PRIMARY KEY,
  employee_id INTEGER NOT NULL,
  created_by INTEGER NOT NULL,
  company_id INTEGER NOT NULL,
  CONSTRAINT fk_document_employee FOREIGN KEY (employee_id) REFERENCES y.employee(id),
  CONSTRAINT fk_document_created_by FOREIGN KEY (created_by) REFERENCES y.employee(id),
  CONSTRAINT fk_document_company FOREIGN KEY (company_id) REFERENCES x.company(id)
); 