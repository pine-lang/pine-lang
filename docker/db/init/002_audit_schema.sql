-- Audit Schema for E-commerce Platform
-- This provides comprehensive audit trails and activity logging

CREATE SCHEMA IF NOT EXISTS audit;

-- User sessions table - track login/logout activity
CREATE TABLE IF NOT EXISTS audit.user_sessions (
  id SERIAL PRIMARY KEY,
  customer_id INTEGER REFERENCES customers(id),
  session_id VARCHAR(255) NOT NULL,
  ip_address INET,
  user_agent TEXT,
  login_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  logout_time TIMESTAMP,
  session_duration_seconds INTEGER,
  login_method VARCHAR(50), -- 'password', 'oauth', 'magic_link'
  is_successful BOOLEAN DEFAULT true,
  failure_reason VARCHAR(255),
  location_country VARCHAR(100),
  location_city VARCHAR(100),
  device_type VARCHAR(50) -- 'desktop', 'mobile', 'tablet'
);

-- Data change audit table - tracks all modifications to core tables
CREATE TABLE IF NOT EXISTS audit.data_changes (
  id SERIAL PRIMARY KEY,
  table_name VARCHAR(100) NOT NULL,
  record_id INTEGER NOT NULL,
  operation VARCHAR(10) NOT NULL, -- 'INSERT', 'UPDATE', 'DELETE'
  old_values JSONB,
  new_values JSONB,
  changed_fields TEXT[], -- array of field names that changed
  changed_by INTEGER REFERENCES customers(id),
  changed_by_type VARCHAR(20) DEFAULT 'customer', -- 'customer', 'admin', 'system'
  change_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  change_reason VARCHAR(255),
  client_ip INET,
  session_id VARCHAR(255)
);

-- Order status changes - detailed tracking of order lifecycle
CREATE TABLE IF NOT EXISTS audit.order_status_changes (
  id SERIAL PRIMARY KEY,
  order_id INTEGER NOT NULL REFERENCES orders(id),
  old_status VARCHAR(20),
  new_status VARCHAR(20) NOT NULL,
  changed_by INTEGER REFERENCES customers(id),
  changed_by_type VARCHAR(20) DEFAULT 'system', -- 'customer', 'admin', 'system', 'warehouse'
  change_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  notes TEXT,
  automated BOOLEAN DEFAULT false,
  notification_sent BOOLEAN DEFAULT false,
  tracking_number VARCHAR(100),
  carrier VARCHAR(50)
);

-- Price changes audit - track product price modifications
CREATE TABLE IF NOT EXISTS audit.price_changes (
  id SERIAL PRIMARY KEY,
  product_id INTEGER NOT NULL REFERENCES products(id),
  old_price DECIMAL(10,2),
  new_price DECIMAL(10,2) NOT NULL,
  price_change_percentage DECIMAL(5,2),
  changed_by INTEGER,
  changed_by_type VARCHAR(20) DEFAULT 'admin',
  change_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  change_reason VARCHAR(255), -- 'promotion', 'cost_increase', 'competitor_price', 'clearance'
  effective_date TIMESTAMP,
  expiry_date TIMESTAMP,
  promotion_id INTEGER
);

-- Inventory movements - track all stock changes
CREATE TABLE IF NOT EXISTS audit.inventory_movements (
  id SERIAL PRIMARY KEY,
  product_id INTEGER NOT NULL REFERENCES products(id),
  movement_type VARCHAR(20) NOT NULL, -- 'restock', 'sale', 'adjustment', 'return', 'damage'
  quantity_change INTEGER NOT NULL, -- positive for increase, negative for decrease
  previous_quantity INTEGER,
  new_quantity INTEGER,
  unit_cost DECIMAL(10,2),
  total_value DECIMAL(10,2),
  reference_type VARCHAR(20), -- 'order', 'purchase', 'adjustment', 'return'
  reference_id INTEGER, -- order_id, purchase_order_id, etc.
  movement_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  warehouse_location VARCHAR(100),
  handled_by VARCHAR(100),
  notes TEXT
);

-- Security events - track suspicious activities and security incidents
CREATE TABLE IF NOT EXISTS audit.security_events (
  id SERIAL PRIMARY KEY,
  event_type VARCHAR(50) NOT NULL, -- 'failed_login', 'password_reset', 'suspicious_order', 'fraud_attempt'
  severity VARCHAR(20) DEFAULT 'medium', -- 'low', 'medium', 'high', 'critical'
  customer_id INTEGER REFERENCES customers(id),
  ip_address INET,
  user_agent TEXT,
  event_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  event_details JSONB,
  risk_score INTEGER, -- 0-100
  blocked BOOLEAN DEFAULT false,
  investigated BOOLEAN DEFAULT false,
  investigation_notes TEXT,
  resolved_at TIMESTAMP,
  false_positive BOOLEAN DEFAULT false
);

-- API access logs - track external API calls and integrations
CREATE TABLE IF NOT EXISTS audit.api_access_logs (
  id SERIAL PRIMARY KEY,
  endpoint VARCHAR(255) NOT NULL,
  http_method VARCHAR(10) NOT NULL,
  request_headers JSONB,
  request_body JSONB,
  response_status INTEGER,
  response_body JSONB,
  response_time_ms INTEGER,
  client_ip INET,
  api_key_id VARCHAR(100),
  customer_id INTEGER REFERENCES customers(id),
  request_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  user_agent TEXT,
  rate_limit_hit BOOLEAN DEFAULT false,
  error_message TEXT
);

-- Payment audit trail - track all payment-related activities
CREATE TABLE IF NOT EXISTS audit.payment_events (
  id SERIAL PRIMARY KEY,
  order_id INTEGER REFERENCES orders(id),
  payment_id VARCHAR(100), -- external payment processor ID
  event_type VARCHAR(30) NOT NULL, -- 'authorized', 'captured', 'failed', 'refunded', 'disputed'
  amount DECIMAL(10,2) NOT NULL,
  currency VARCHAR(3) DEFAULT 'USD',
  payment_method VARCHAR(50),
  processor VARCHAR(50), -- 'stripe', 'paypal', 'square'
  processor_response JSONB,
  event_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  customer_id INTEGER REFERENCES customers(id),
  risk_assessment JSONB,
  fees DECIMAL(10,2),
  net_amount DECIMAL(10,2),
  gateway_transaction_id VARCHAR(255)
);

-- Email notifications audit - track all email communications
CREATE TABLE IF NOT EXISTS audit.email_notifications (
  id SERIAL PRIMARY KEY,
  customer_id INTEGER REFERENCES customers(id),
  email_type VARCHAR(50) NOT NULL, -- 'order_confirmation', 'shipping_update', 'password_reset', 'promotion'
  email_address VARCHAR(255) NOT NULL,
  subject VARCHAR(255),
  template_name VARCHAR(100),
  template_variables JSONB,
  sent_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  delivery_status VARCHAR(20) DEFAULT 'pending', -- 'pending', 'sent', 'delivered', 'bounced', 'failed'
  opened BOOLEAN DEFAULT false,
  clicked BOOLEAN DEFAULT false,
  first_opened_at TIMESTAMP,
  click_count INTEGER DEFAULT 0,
  bounce_reason VARCHAR(255),
  external_message_id VARCHAR(255)
);

-- Now let's insert realistic audit data

-- User sessions
INSERT INTO audit.user_sessions (customer_id, session_id, ip_address, user_agent, login_time, logout_time, session_duration_seconds, login_method, location_country, location_city, device_type) VALUES
(1, 'sess_1a2b3c4d', '192.168.1.100', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36', '2024-01-25 09:15:00', '2024-01-25 10:45:00', 5400, 'password', 'USA', 'New York', 'desktop'),
(2, 'sess_2e3f4g5h', '10.0.0.25', 'Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X)', '2024-01-25 14:30:00', '2024-01-25 14:45:00', 900, 'oauth', 'USA', 'Los Angeles', 'mobile'),
(3, 'sess_3i4j5k6l', '172.16.0.50', 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)', '2024-01-24 19:20:00', '2024-01-24 20:10:00', 3000, 'password', 'USA', 'Chicago', 'desktop'),
(1, 'sess_4m5n6o7p', '192.168.1.100', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36', '2024-01-26 08:00:00', NULL, NULL, 'password', 'USA', 'New York', 'desktop'),
(4, 'sess_5q6r7s8t', '203.0.113.45', 'Mozilla/5.0 (Android 13; Mobile)', '2024-01-25 16:45:00', '2024-01-25 17:30:00', 2700, 'magic_link', 'USA', 'Houston', 'mobile'),
(5, 'sess_failed1', '198.51.100.20', 'Mozilla/5.0 (X11; Linux x86_64)', '2024-01-25 11:30:00', NULL, NULL, 'password', 'USA', 'Phoenix', 'desktop')
ON CONFLICT DO NOTHING;

-- Update the last session to show a failed login
UPDATE audit.user_sessions SET is_successful = false, failure_reason = 'Invalid password' WHERE session_id = 'sess_failed1';

-- Data changes audit
INSERT INTO audit.data_changes (table_name, record_id, operation, old_values, new_values, changed_fields, changed_by, change_timestamp, change_reason, client_ip) VALUES
('customers', 1, 'UPDATE', '{"email": "john.doe@email.com", "phone": null}', '{"email": "john.doe@email.com", "phone": "+1-555-0101"}', ARRAY['phone'], 1, '2024-01-20 15:30:00', 'Customer updated profile', '192.168.1.100'),
('products', 1, 'UPDATE', '{"price": 2599.00}', '{"price": 2499.00}', ARRAY['price'], NULL, '2024-01-15 09:00:00', 'Promotional pricing', '10.0.0.1'),
('orders', 1, 'INSERT', NULL, '{"customer_id": 1, "total_amount": 2698.92, "status": "pending"}', NULL, 1, '2024-01-15 10:30:00', 'New order created', '192.168.1.100'),
('product_inventory', 1, 'UPDATE', '{"quantity_available": 27}', '{"quantity_available": 25}', ARRAY['quantity_available'], NULL, '2024-01-15 10:30:00', 'Order fulfillment', '10.0.0.1');

-- Order status changes
INSERT INTO audit.order_status_changes (order_id, old_status, new_status, changed_by_type, change_timestamp, notes, automated, notification_sent, tracking_number, carrier) VALUES
(1, 'pending', 'confirmed', 'system', '2024-01-15 10:35:00', 'Payment confirmed', true, true, NULL, NULL),
(1, 'confirmed', 'shipped', 'warehouse', '2024-01-16 09:00:00', 'Package dispatched from warehouse', false, true, 'TRK123456789', 'FedEx'),
(1, 'shipped', 'delivered', 'system', '2024-01-18 14:30:00', 'Package delivered to customer', true, true, 'TRK123456789', 'FedEx'),
(2, 'pending', 'confirmed', 'system', '2024-01-18 14:20:00', 'Payment confirmed', true, true, NULL, NULL),
(2, 'confirmed', 'shipped', 'warehouse', '2024-01-19 11:30:00', 'Package dispatched', false, true, 'TRK987654321', 'UPS'),
(2, 'shipped', 'delivered', 'system', '2024-01-22 16:45:00', 'Package delivered', true, true, 'TRK987654321', 'UPS'),
(3, 'pending', 'confirmed', 'system', '2024-01-20 09:50:00', 'Payment confirmed', true, true, NULL, NULL),
(3, 'confirmed', 'shipped', 'warehouse', '2024-01-21 08:15:00', 'Package shipped', false, true, 'TRK456789123', 'DHL'),
(7, 'pending', 'cancelled', 'customer', '2024-01-10 16:00:00', 'Customer requested cancellation', false, true, NULL, NULL);

-- Price changes
INSERT INTO audit.price_changes (product_id, old_price, new_price, price_change_percentage, changed_by_type, change_timestamp, change_reason, effective_date) VALUES
(1, 2599.00, 2499.00, -3.85, 'admin', '2024-01-15 09:00:00', 'promotion', '2024-01-15 00:00:00'),
(3, 1099.00, 999.00, -9.10, 'admin', '2024-01-10 10:00:00', 'competitor_price', '2024-01-12 00:00:00'),
(5, 379.00, 349.00, -7.92, 'admin', '2024-01-08 14:00:00', 'clearance', '2024-01-10 00:00:00'),
(9, 29.99, 24.99, -16.67, 'admin', '2024-01-05 12:00:00', 'promotion', '2024-01-05 00:00:00');

-- Inventory movements
INSERT INTO audit.inventory_movements (product_id, movement_type, quantity_change, previous_quantity, new_quantity, reference_type, reference_id, movement_timestamp, warehouse_location, handled_by, notes) VALUES
(1, 'sale', -1, 26, 25, 'order', 1, '2024-01-15 10:30:00', 'Main Warehouse', 'auto_system', 'Order fulfillment'),
(5, 'sale', -1, 41, 40, 'order', 2, '2024-01-18 14:15:00', 'Main Warehouse', 'auto_system', 'Order fulfillment'),
(6, 'sale', -1, 61, 60, 'order', 2, '2024-01-18 14:15:00', 'Main Warehouse', 'auto_system', 'Order fulfillment'),
(2, 'sale', -1, 19, 18, 'order', 3, '2024-01-20 09:45:00', 'Main Warehouse', 'auto_system', 'Order fulfillment'),
(1, 'restock', 50, 5, 55, 'purchase', 1001, '2024-01-15 10:00:00', 'Main Warehouse', 'warehouse_staff', 'New shipment received'),
(3, 'restock', 30, 35, 65, 'purchase', 1002, '2024-01-20 09:15:00', 'Main Warehouse', 'warehouse_staff', 'Restock from supplier'),
(7, 'adjustment', -2, 14, 12, 'adjustment', NULL, '2024-01-22 16:00:00', 'Main Warehouse', 'warehouse_manager', 'Found damaged items during audit');

-- Security events
INSERT INTO audit.security_events (event_type, severity, customer_id, ip_address, user_agent, event_timestamp, event_details, risk_score, blocked, investigated) VALUES
('failed_login', 'medium', 5, '198.51.100.20', 'Mozilla/5.0 (X11; Linux x86_64)', '2024-01-25 11:30:00', '{"attempts": 3, "reason": "invalid_password"}', 45, false, false),
('suspicious_order', 'high', 4, '203.0.113.45', 'Mozilla/5.0 (Android 13; Mobile)', '2024-01-22 16:20:00', '{"order_value": 126.37, "shipping_mismatch": true, "new_customer": true}', 75, false, true),
('password_reset', 'low', 2, '10.0.0.25', 'Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X)', '2024-01-24 10:15:00', '{"method": "email", "token_used": true}', 15, false, false),
('fraud_attempt', 'critical', NULL, '192.0.2.100', 'curl/7.68.0', '2024-01-23 03:45:00', '{"payment_declined": true, "velocity_check_failed": true, "multiple_cards": true}', 95, true, true);

-- API access logs
INSERT INTO audit.api_access_logs (endpoint, http_method, response_status, response_time_ms, client_ip, customer_id, request_timestamp, user_agent, rate_limit_hit) VALUES
('/api/v1/products', 'GET', 200, 145, '192.168.1.100', 1, '2024-01-25 09:16:00', 'ecommerce-app/1.0', false),
('/api/v1/orders', 'POST', 201, 320, '192.168.1.100', 1, '2024-01-15 10:30:00', 'ecommerce-app/1.0', false),
('/api/v1/customers/profile', 'PUT', 200, 89, '10.0.0.25', 2, '2024-01-25 14:32:00', 'mobile-app/2.1', false),
('/api/v1/products/search', 'GET', 200, 67, '172.16.0.50', 3, '2024-01-24 19:25:00', 'web-browser', false),
('/api/v1/orders', 'GET', 429, 12, '203.0.113.100', NULL, '2024-01-26 15:30:00', 'scraper-bot/1.0', true);

-- Payment events
INSERT INTO audit.payment_events (order_id, payment_id, event_type, amount, payment_method, processor, event_timestamp, customer_id, fees, net_amount, gateway_transaction_id) VALUES
(1, 'pi_1234567890', 'authorized', 2698.92, 'credit_card', 'stripe', '2024-01-15 10:30:00', 1, 78.27, 2620.65, 'txn_stripe_abc123'),
(1, 'pi_1234567890', 'captured', 2698.92, 'credit_card', 'stripe', '2024-01-15 10:35:00', 1, 78.27, 2620.65, 'txn_stripe_abc123'),
(2, 'PAYPAL789456', 'authorized', 611.83, 'paypal', 'paypal', '2024-01-18 14:15:00', 2, 18.35, 593.48, 'txn_paypal_def456'),
(2, 'PAYPAL789456', 'captured', 611.83, 'paypal', 'paypal', '2024-01-18 14:20:00', 2, 18.35, 593.48, 'txn_paypal_def456'),
(7, 'pi_0987654321', 'authorized', 95.38, 'credit_card', 'stripe', '2024-01-10 15:30:00', 6, 2.86, 92.52, 'txn_stripe_ghi789'),
(7, 'pi_0987654321', 'refunded', 95.38, 'credit_card', 'stripe', '2024-01-10 16:05:00', 6, -2.86, -92.52, 'txn_stripe_ghi789');

-- Email notifications
INSERT INTO audit.email_notifications (customer_id, email_type, email_address, subject, template_name, sent_timestamp, delivery_status, opened, clicked, first_opened_at, click_count) VALUES
(1, 'order_confirmation', 'john.doe@email.com', 'Order Confirmation - Order #ORD-2024-001', 'order_confirmation', '2024-01-15 10:35:00', 'delivered', true, true, '2024-01-15 11:20:00', 2),
(1, 'shipping_update', 'john.doe@email.com', 'Your order has shipped!', 'shipping_notification', '2024-01-16 09:05:00', 'delivered', true, true, '2024-01-16 14:30:00', 1),
(2, 'order_confirmation', 'jane.smith@email.com', 'Order Confirmation - Order #ORD-2024-002', 'order_confirmation', '2024-01-18 14:20:00', 'delivered', true, false, '2024-01-18 16:45:00', 0),
(3, 'order_confirmation', 'bob.johnson@email.com', 'Order Confirmation - Order #ORD-2024-003', 'order_confirmation', '2024-01-20 09:50:00', 'delivered', false, false, NULL, 0),
(6, 'password_reset', 'diana.garcia@email.com', 'Password Reset Request', 'password_reset', '2024-01-24 10:15:00', 'delivered', true, true, '2024-01-24 10:18:00', 1),
(4, 'promotion', 'alice.brown@email.com', 'Special offer just for you!', 'promotional', '2024-01-25 08:00:00', 'bounced', false, false, NULL, 0);

-- Update the bounced email with bounce reason
UPDATE audit.email_notifications SET bounce_reason = 'Invalid email address' WHERE email_type = 'promotion' AND customer_id = 4;

-- Create indexes for better query performance on audit tables
CREATE INDEX IF NOT EXISTS idx_audit_sessions_customer ON audit.user_sessions(customer_id);
CREATE INDEX IF NOT EXISTS idx_audit_sessions_timestamp ON audit.user_sessions(login_time);
CREATE INDEX IF NOT EXISTS idx_audit_data_changes_table ON audit.data_changes(table_name);
CREATE INDEX IF NOT EXISTS idx_audit_data_changes_timestamp ON audit.data_changes(change_timestamp);
CREATE INDEX IF NOT EXISTS idx_audit_order_status_order ON audit.order_status_changes(order_id);
CREATE INDEX IF NOT EXISTS idx_audit_price_changes_product ON audit.price_changes(product_id);
CREATE INDEX IF NOT EXISTS idx_audit_inventory_product ON audit.inventory_movements(product_id);
CREATE INDEX IF NOT EXISTS idx_audit_security_events_timestamp ON audit.security_events(event_timestamp);
CREATE INDEX IF NOT EXISTS idx_audit_api_logs_timestamp ON audit.api_access_logs(request_timestamp);
CREATE INDEX IF NOT EXISTS idx_audit_payment_events_order ON audit.payment_events(order_id);
CREATE INDEX IF NOT EXISTS idx_audit_emails_customer ON audit.email_notifications(customer_id);

-- Create some useful views for common audit queries
CREATE OR REPLACE VIEW audit.recent_user_activity AS
SELECT 
    c.first_name || ' ' || c.last_name as customer_name,
    c.email,
    us.login_time,
    us.logout_time,
    us.device_type,
    us.location_city,
    CASE WHEN us.logout_time IS NOT NULL 
         THEN us.session_duration_seconds 
         ELSE EXTRACT(EPOCH FROM (CURRENT_TIMESTAMP - us.login_time))::INTEGER 
    END as session_duration_seconds,
    us.is_successful
FROM audit.user_sessions us
JOIN customers c ON us.customer_id = c.id
WHERE us.login_time >= CURRENT_DATE - INTERVAL '7 days'
ORDER BY us.login_time DESC;

CREATE OR REPLACE VIEW audit.order_lifecycle AS
SELECT 
    o.order_number,
    c.first_name || ' ' || c.last_name as customer_name,
    osc.old_status,
    osc.new_status,
    osc.change_timestamp,
    osc.changed_by_type,
    osc.tracking_number,
    osc.carrier,
    osc.notes
FROM audit.order_status_changes osc
JOIN orders o ON osc.order_id = o.id
JOIN customers c ON o.customer_id = c.id
ORDER BY o.order_number, osc.change_timestamp; 