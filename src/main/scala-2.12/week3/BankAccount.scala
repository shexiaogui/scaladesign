package week3

import week4.frp.Var


/**
  * Created by shexiaogui on 09/04/17.
  */
class BankAccount {
  val balance = Var(0)
  
  def deposit(amount: Int): Unit = {
    if(amount > 0) {val b = balance(); balance() = b + amount}
  }
  def withdraw(amount: Int): Int = {
    if(amount >= 0 && balance()  >= amount){
      val b = balance()
      balance() = b - amount
      balance()
    }
    else throw new Error("no enough balance in account")
  }
}
