a = 1 -- 기본적인 변수 선언법 하스켈의 변수는 불변하다.(immutable)
b = 12

area r = pi * r ^ 2 -- 원 넓이를 구하는 함수
double x    = 2 * x -- x를 2배로 만드는 함수
quadruple x = double (double x) -- x를 4배로 만드는 함수
square x    = x * x -- 제곱을 구하는 함수
half   x    = x / 2 -- 2로 나누는 함수
foo x = half x - 12 -- x를 2로 나누고 12를 뺀다.


main = do
  -- 함수 호출법 괄호 없이 파라미터 값을 입력 할 수 있다.
  print (area 2)

  -- 15 + 2 = 17 (곱셈을 덧셈보다 먼저)
  print (5 * 3 + 2)

  -- 5 * 5 = 25 (괄호 덕분에)
  print (5 * (3 + 2))

  {--
    (area 5) * 3 = 75
    하스켈에서는 함수 호출이 연산자보다 우선순위가 높다.
    수학에서 곱셈이 덧셈보다 우선순위가 더 높은 것과 동일
  --}
  print (area 5 * 3)

  -- area 15 = 706.8583470577034
  print (area (5 * 3))

  --
  print (foo 5)
