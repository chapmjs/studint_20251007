-- Create database (run this first if database doesn't exist)
CREATE DATABASE IF NOT EXISTS student_interactions;

USE student_interactions;

-- Students table
CREATE TABLE IF NOT EXISTS students (
    student_id INT AUTO_INCREMENT PRIMARY KEY,
    first_name VARCHAR(100) NOT NULL,
    last_name VARCHAR(100) NOT NULL,
    phone VARCHAR(20),
    email VARCHAR(255),
    graduation_month VARCHAR(20),
    graduation_year INT,
    hometown VARCHAR(100),
    major VARCHAR(100),
    linkedin_url VARCHAR(255),
    social_media TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    INDEX idx_name (last_name, first_name),
    INDEX idx_grad_year (graduation_year)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Interactions table
CREATE TABLE IF NOT EXISTS interactions (
    interaction_id INT AUTO_INCREMENT PRIMARY KEY,
    student_id INT NOT NULL,
    interaction_date DATE NOT NULL,
    interaction_time TIME,
    location VARCHAR(255),
    notes TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (student_id) REFERENCES students(student_id) ON DELETE CASCADE,
    INDEX idx_student (student_id),
    INDEX idx_date (interaction_date)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
