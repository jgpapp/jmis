package com.jgp.bmo.dto;

import java.io.Serializable;
import java.time.LocalDate;

public record MentorshipResponseDto(
         Long id,

         Long partnerId,

         String participantJGPID,

         String partnerName,

         Long participantId,

         String participantName,

         String businessName,

         LocalDate mentorShipDate,

         String mentorShipOrganization,

         String bmoMemberShip,

         String msmeSessionsCovered,

         String smeSessionsCovered,

         String uploadedBy,

         String dateUploaded,

         String approvedBy,

         LocalDate dateApproved
) implements Serializable {}
