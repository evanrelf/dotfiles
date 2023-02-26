hook -once global KakBegin .* %{
  require-module "number-toggle"
  set-option global number_toggle_params -hlcursor
}
