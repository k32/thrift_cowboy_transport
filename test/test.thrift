struct Vec2
{
  1: double x,
  2: double y,
}

// Define services
service TestService {
  void ping(),
  double test_v2(1:Vec2 v),
}
