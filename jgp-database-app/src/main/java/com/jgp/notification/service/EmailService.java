package com.jgp.notification.service;


public interface EmailService {

    void sendEmailNotification(String to, String subject, String content);

    void sendEmailNotificationForDataReview(String toEmail, String toName, String dataType, String appDomain);
}
