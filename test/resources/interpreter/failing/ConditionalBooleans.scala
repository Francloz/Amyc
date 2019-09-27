object ConditionalBooleans {
  if((true && false) || true)
    if(!(true && (true && false)))
      error("")
}
