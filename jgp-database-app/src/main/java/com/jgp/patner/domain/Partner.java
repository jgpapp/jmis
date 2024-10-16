package com.jgp.patner.domain;


import com.jgp.patner.dto.PartnerDto;
import com.jgp.shared.domain.BaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

@Getter
@Entity
@Table(name = "partners", uniqueConstraints = { @UniqueConstraint(columnNames = { "partner_name" }, name = "NAME_UNIQUE")})
public class Partner extends BaseEntity {

    @Column(name = "partner_name")
	private String partnerName;

    @Column(name = "type")
    @Enumerated(EnumType.STRING)
    private PartnerType type;

    public Partner() {
    }

    private Partner(String partnerName, PartnerType type) {
        this.partnerName = partnerName;
        this.type = type;
    }

    public static Partner createPartner(PartnerDto partnerDto){
        return new Partner(partnerDto.partnerName(), PartnerType.valueOf(partnerDto.type()));
    }

    public void updatePartner(PartnerDto partnerDto){
        if(!StringUtils.equals(partnerDto.partnerName(), this.partnerName)){
            this.partnerName = partnerDto.partnerName();
        }
        if(!StringUtils.equals(partnerDto.type(), this.type.name())){
            this.type = PartnerType.valueOf(partnerDto.type());
        }
    }


    @Getter
    public enum PartnerType {

        BMO("BMO"),
        FI("Financial Intermediary"),
        JGP("JGP");

        private final String name;

        PartnerType(String name) {
            this.name = name;
        }

    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        Partner partner = (Partner) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), partner.getId())
                .append(getPartnerName(), partner.getPartnerName())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getPartnerName()).toHashCode();
    }
}
