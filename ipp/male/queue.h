#ifndef IPP_MALE_QUEUE_H
#define IPP_MALE_QUEUE_H

struct SimplerQueueTag;
typedef struct SimplerQueueTag SimplerQueue;

extern SimplerQueue *create_queue();

extern void push_to_queue(SimplerQueue *q, size_t value);

extern size_t get_from_queue(SimplerQueue *q);

extern int is_queue_empty(SimplerQueue *q);

extern void switch_queues(SimplerQueue **q1, SimplerQueue **q2);

extern void delete_queue(SimplerQueue *q);

#endif //IPP_MALE_QUEUE_H
