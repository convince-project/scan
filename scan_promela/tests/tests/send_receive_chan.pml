chan q = [1] of { int }

proctype P() { 
    q!5 
}
proctype Q() {
    int x;
    q?x
}