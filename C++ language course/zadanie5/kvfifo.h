//
// Created by Maciej Kielak, Mateusz Piekarczyk and Wojciech Rzepliński
//

#ifndef ASD_KVFIFO_H
#define ASD_KVFIFO_H

#include <iostream>
#include <list>
#include <map>
#include <memory>

using std::cin, std::cout, std::endl;

template<typename K, typename V>
class kvfifo {
private:

    using _list_t = std::list<std::pair<K, V>>;
    using _list_iterator_t = typename _list_t::iterator;
    using _map_t = std::map<K, std::list<_list_iterator_t>>;
    using _map_iterator_t = typename _map_t::const_iterator;

    std::shared_ptr<_list_t> list_ptr;
    std::shared_ptr<_map_t> map_ptr;
    bool exists_reference = false;

    void copy_if_needed() {
        if (!map_ptr.unique()) {
            copy();
        }
    }

    void copy() {
        std::shared_ptr<_list_t> list_ptr_copy = std::make_shared<_list_t>(*list_ptr);
        std::shared_ptr<_map_t> map_ptr_copy = std::make_shared<_map_t>();
        _list_iterator_t it;

        for (it = (*list_ptr_copy).begin(); it != (*list_ptr_copy).end(); ++it) {
            (*map_ptr_copy)[(*it).first].push_back(it);
        }

        map_ptr = map_ptr_copy;
        list_ptr = list_ptr_copy;
        exists_reference = false;
    }

    void check_if_empty() const {
        if (empty())
            throw std::invalid_argument("The queue is empty");
    }

    void check_if_key_exists(K const &key) const {
        if (count(key) == 0)
            throw std::invalid_argument("No element with given key");
    }

public:

    class k_iterator : public _map_iterator_t {
    public:
        k_iterator() : _map_iterator_t() {};

        explicit k_iterator(_map_iterator_t i) : _map_iterator_t(i) {};

        K *operator->() { return (K *const) &(_map_iterator_t::operator->()->first); }

        K operator*() { return _map_iterator_t::operator*().first; }
    };

    k_iterator k_begin() const noexcept {
        return k_iterator(map_ptr->cbegin());
    }

    k_iterator k_end() const noexcept { return k_iterator(map_ptr->cend()); }

    kvfifo() {
        map_ptr = std::make_shared<_map_t>();
        list_ptr = std::make_shared<_list_t>();
    }

    kvfifo(kvfifo const &other) {
        map_ptr = other.map_ptr;
        list_ptr = other.list_ptr;

        if (other.exists_reference) {
            copy();
        }
    };


    kvfifo(kvfifo &&other) noexcept :
            list_ptr(std::move(other.list_ptr)),
            map_ptr(std::move(other.map_ptr)),
            exists_reference(std::move(other.exists_reference)) {
        other.list_ptr = std::make_shared<_list_t>();
        other.map_ptr = std::make_shared<_map_t>();
    }


    kvfifo &operator=(kvfifo other) {
        if (map_ptr.get() == other.map_ptr.get() && list_ptr.get() == other.list_ptr.get()) return *this;
        std::shared_ptr<_map_t> old_map_ptr = map_ptr;
        std::shared_ptr<_list_t> old_list_ptr = list_ptr;

        try {
            map_ptr = other.map_ptr;
            list_ptr = other.list_ptr;
            if (other.exists_reference) {
                copy();
            }
            exists_reference = false;
            return *this;
        }
        catch (...) {
            map_ptr = old_map_ptr;
            list_ptr = old_list_ptr;
            throw;
        }

    }

    void push(K const &k, V const &v) {
        copy_if_needed();

        list_ptr->push_back({k, v});
        _list_iterator_t last_elem_iterator = list_ptr->end();
        try {
            (*map_ptr)[k].push_back(prev(last_elem_iterator));

            exists_reference = false;

        } catch (...) {
            list_ptr->erase(prev(last_elem_iterator));
            if ((*map_ptr)[k].empty()) {
                map_ptr->erase(k);
            }
            throw;
        }
    }

    void move_to_back(K const &k) {
        check_if_key_exists(k);
        copy_if_needed();

        for (auto it: (*map_ptr)[k])
            list_ptr->splice(list_ptr->end(), (*list_ptr), it);

        exists_reference = false;
    }

    void pop() {
        check_if_empty();
        copy_if_needed();

        auto elem = list_ptr->front();
        list_ptr->pop_front();
        (*map_ptr)[elem.first].pop_front();
        if ((*map_ptr)[elem.first].empty()) {
            map_ptr->erase(elem.first);
        }

        exists_reference = false;
    }

    void pop(K const &k) {
        check_if_key_exists(k);
        copy_if_needed();

        _list_iterator_t elem_iter = (*map_ptr)[k].front();
        (*map_ptr)[k].pop_front();
        list_ptr->erase(elem_iter);
        if ((*map_ptr)[k].empty()) {
            map_ptr->erase(k);
        }
        exists_reference = false;
    }

    std::pair<K const &, V &> front() {
        check_if_empty();
        copy_if_needed();

        exists_reference = true;
        return {list_ptr->front().first, list_ptr->front().second};
    }

    std::pair<K const &, V const &> front() const {
        check_if_empty();
        return {list_ptr->front().first, list_ptr->front().second};
    }

    std::pair<K const &, V &> back() {
        check_if_empty();
        copy_if_needed();

        exists_reference = true;
        return {list_ptr->back().first, list_ptr->back().second};
    }

    std::pair<K const &, V const &> back() const {
        check_if_empty();
        return {list_ptr->back().first, list_ptr->back().second};
    }

    std::pair<K const &, V &> first(K const &key) {
        check_if_key_exists(key);
        copy_if_needed();

        exists_reference = true;
        _list_iterator_t elem_iter = (*map_ptr)[key].front();
        return {elem_iter->first, elem_iter->second};
    }

    std::pair<K const &, V const &> first(K const &key) const {
        check_if_key_exists(key);
        _list_iterator_t elem_iter = (*map_ptr)[key].front();
        return {elem_iter->first, elem_iter->second};
    };

    std::pair<K const &, V &> last(K const &key) {
        check_if_key_exists(key);
        copy_if_needed();

        exists_reference = true;
        _list_iterator_t elem_iter = (*map_ptr)[key].back();
        return {elem_iter->first, elem_iter->second};
    };

    std::pair<K const &, V const &> last(K const &key) const {
        check_if_key_exists(key);
        _list_iterator_t elem_iter = (*map_ptr)[key].back();
        return {elem_iter->first, elem_iter->second};
    };

    size_t size() const noexcept {
        if (!list_ptr)
            return 0;
        return list_ptr->size();
    }

    bool empty() const noexcept {
        if (!list_ptr)
            return true;
        return (list_ptr->empty());
    }

    size_t count(K const &key) const noexcept {
        if (!map_ptr || !(map_ptr->contains(key)))
            return 0;
        return (*map_ptr)[key].size();
    };

    void clear() {
        map_ptr = std::make_shared<_map_t>();
        list_ptr = std::make_shared<_list_t>();
    };

};

#endif //ASD_KVFIFO_H
