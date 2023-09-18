#ifdef NDEBUG
#undef NDEBUG
#endif

#include <assert.h>
#include <string.h>
#include "../phone_forward.h"
#include "../linked_list.h"

#define MAX_LEN 23

bool is_phfwdAdd_correct(PhoneForward *pf, const char *num, const char *num2) {
    PhoneNumbers *pn;
    pn = phfwdGet(pf, num);
    bool r = false;
    r = strcmp(phnumGet(pn, 0), num2) == 0;
    phnumDelete(pn);
    return r;
}

int my_tests() {
    PhoneForward *pf = phfwdNew();

    assert(phfwdAdd(pf, "123", "") == false);
    assert(phfwdAdd(pf, "123", "123") == false);
    phfwdAdd(pf, "5", "6");
    phfwdAdd(pf, "66", "33");
    phfwdAdd(pf, "2", "000002");
    phfwdAdd(pf, "333", "222");
    phfwdAdd(pf, "4", "2");
    phfwdAdd(pf, "42", "21");
    phfwdAdd(pf, "420", "210");
    phfwdAdd(pf, "4206", "2103");

    phnumGet(NULL, 2);

    assert(is_phfwdAdd_correct(pf, "43", "23"));
    assert(is_phfwdAdd_correct(pf, "423", "213"));
    assert(is_phfwdAdd_correct(pf, "4203", "2103"));
    assert(is_phfwdAdd_correct(pf, "42063", "21033"));

    phfwdRemove(pf, "420");

    assert(is_phfwdAdd_correct(pf, "43", "23"));
    assert(is_phfwdAdd_correct(pf, "423", "213"));
    assert(is_phfwdAdd_correct(pf, "4203", "2103"));
    assert(is_phfwdAdd_correct(pf, "42063", "21063"));

    phfwdDelete(pf);
    return 0;
}

int main() {

    PhoneForward *pf = phfwdNew();
    phfwdAdd(pf, "123", "456");
    phfwdAdd(pf, "*999", "456");
    phfwdAdd(pf, "#888", "456");
    PhoneNumbers *pn = phfwdGet(pf, "1231");
    printf("%s\n", phnumGet(pn, 0));
    phnumDelete(pn);

    pn = phfwdReverse(pf, "456111");
    printf("%s ", phnumGet(pn, 0));
    printf("%s ", phnumGet(pn, 1));
    printf("%s ", phnumGet(pn, 2));
    printf("%s\n", phnumGet(pn, 3));
    phnumDelete(pn);


    //Testing same values
    phfwdAdd(pf, "1", "2");
    phfwdAdd(pf, "999", "2");
    phfwdAdd(pf, "12", "22");
    phfwdAdd(pf, "888", "22");
    phfwdAdd(pf, "123", "223");


    pn = phfwdReverse(pf, "223");
    printf("%s ", phnumGet(pn, 0));
    printf("%s ", phnumGet(pn, 1));
    printf("%s ", phnumGet(pn, 2));
    printf("%s\n", phnumGet(pn, 3));
    phnumDelete(pn);

    phfwdRemove(pf, "");
    phfwdAdd(pf, "99", "12");
    phfwdAdd(pf, "99", "34");

    pn = phfwdReverse(pf, "12");
    printf("%s ", phnumGet(pn, 0));
    phnumDelete(pn);


    phfwdDelete(pf);
    my_tests();
    return 0;
}
