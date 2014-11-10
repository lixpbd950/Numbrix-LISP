/**
 * Definition for singly-linked list.
 * class ListNode {
 *     int val;
 *     ListNode next;
 *     ListNode(int x) {
 *         val = x;
 *         next = null;
 *     }
 * }
 */

public class LinkedListCircle2 {
    public ListNode detectCycle(ListNode head) {
        ListNode walker = head;
        ListNode runner = head;
        if(head==null)
                return null;
        while(true){
            walker = walker.next;
            if(runner.next==null)
                return null;
            else{
                runner = runner.next.next;
            }
            if(walker==null || runner==null)
                return null;
            if(walker==runner){
                runner = head;
                while(runner!=walker){
                    runner = runner.next;
                    walker = walker.next;
                }
                return walker;
            }
    }
}
}