package com.jgp;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.core.task.TaskExecutor;
import org.springframework.data.web.config.EnableSpringDataWebSupport;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import static org.springframework.data.web.config.EnableSpringDataWebSupport.PageSerializationMode.VIA_DTO;

@SpringBootApplication
@EnableSpringDataWebSupport(pageSerializationMode = VIA_DTO)
@EnableAsync
public class JgpDatabaseAppApplication {

	public static void main(String[] args) {
		SpringApplication.run(JgpDatabaseAppApplication.class, args);
	}

	@Bean(name = "taskExecutor")
	public TaskExecutor taskExecutor() {
		ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
		executor.setCorePoolSize(10);  // Minimum number of threads
		executor.setMaxPoolSize(20);  // Maximum number of threads
		executor.setQueueCapacity(25);  // Queue size
		executor.setThreadNamePrefix("Async-");  // Thread name prefix
		executor.initialize();  // Initialize the executor
		return executor;
	}

}
