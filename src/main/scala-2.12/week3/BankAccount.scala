package week3

/**
  * Created by shexiaogui on 09/04/17.
  */
class BankAccount {
  private var balance = 0
  
  def deposit(amount: Int): Unit = {
    if(amount > 0) balance = balance + amount
  }
  def withdraw(amount: Int): Int = {
    if(amount >= 0 && balance  >= amount){
      balance = balance - amount; balance
    }
    else throw new Error("no enough balance in account")
  }
}
