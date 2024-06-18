provider "aws" {
  region = "us-west-2"
}

data "aws_ami" "amazon_linux" {
  most_recent = true

  filter {
    name   = "name"
    values = ["amzn2-ami-hvm-*-x86_64-gp2"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["amazon"]
}

resource "aws_launch_configuration" "chat_server" {
  name          = "chat_server_configuration"
  image_id      = data.aws_ami.amazon_linux.id
  instance_type = "t2.micro"

  lifecycle {
    create_before_destroy = true
  }
}

resource "aws_autoscaling_group" "chat_server" {
  launch_configuration = aws_launch_configuration.chat_server.id
  min_size             = 1
  max_size             = 3
  desired_capacity     = 2
  vpc_zone_identifier  = ["<your-subnet-id>"]

  tag {
    key                 = "Name"
    value               = "ChatServer"
    propagate_at_launch = true
  }
}

resource "aws_elb" "chat_server" {
  name               = "chat-server-elb"
  subnets            = ["<your-subnet-id>"]
  security_groups    = ["<your-security-group-id>"]

  listener {
    instance_port     = 80
    instance_protocol = "http"
    lb_port           = 80
    lb_protocol       = "http"
  }

  health_check {
    healthy_threshold   = 2
    unhealthy_threshold = 2
    timeout             = 3
    target              = "HTTP:80/"
    interval            = 30
  }
}

resource "aws_autoscaling_attachment" "chat_server" {
  autoscaling_group_name = aws_autoscaling_group.chat_server.id
  elb                    = aws_elb.chat_server.id
}