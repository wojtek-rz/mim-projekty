//
// Created by Antoni Grodowski, Krzysztof Olszak and Wojciech Rzepliński
//

#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <algorithm>
#include <regex>
#include <string>
#include <sstream>

using song_t = unsigned int;
using songs_set = std::unordered_set<song_t>;
using votes_map = std::unordered_map<song_t, unsigned>;
using chart_vector = std::vector<song_t>;

const song_t MAX_SONG = 99999999;
const size_t CHART_SIZE = 7;
const std::regex vote_regex = std::regex("\\s*(\\d+\\s*)+");
const std::regex new_max_regex = std::regex("^\\s*NEW\\s+\\d+\\s*$");
const std::regex top_regex = std::regex("^\\s*TOP\\s*$");
const std::regex blank_line_regex = std::regex("^\\s*$");

int find_index_in_vector(chart_vector &v, song_t s) {
    auto pos = find(v.begin(), v.end(), s);
    if (pos == v.end()) return -1;
    else return int(pos - v.begin());
}

void print_chart(chart_vector &prev_chart, chart_vector &new_chart) {
    for (size_t i = 0; i < new_chart.size(); i++) {
        std::cout << new_chart[i] << " ";
        int prev_position = find_index_in_vector(prev_chart, new_chart[i]);
        if (prev_position != -1) {
            std::cout << prev_position - int(i) << std::endl;
        } else {
            std::cout << "-" << std::endl;
        }
    }
}

void push_best_songs_to_vector(size_t how_many, votes_map votes, chart_vector &new_chart) {
    while (!votes.empty() && new_chart.size() < how_many) {
        auto top_song = max_element
                (
                        votes.begin(), votes.end(),
                        [](const std::pair<song_t, unsigned> &a, const std::pair<song_t, unsigned> &b) {
                            if (a.second == b.second) return a.first > b.first;
                            return a.second < b.second;
                        }
                );

        new_chart.push_back(top_song->first);
        votes.erase(top_song);
    }
}

void add_votes_to_top_votes(votes_map &top_votes, chart_vector &chart) {
    for (size_t i = 0; i < chart.size(); i++) {
        top_votes[chart[i]] += CHART_SIZE - i;
    }
}

void add_to_old_hits(chart_vector &prev_chart, chart_vector &new_chart, songs_set &old_hits) {
    for (song_t song: prev_chart) {
        if (find_index_in_vector(new_chart, song) == -1) {
            old_hits.insert(song);
        }
    }
}

void handle_new_max(votes_map &votes, songs_set &old_hits, chart_vector &prev_chart, votes_map &top_votes) {
    chart_vector new_chart;

    push_best_songs_to_vector(CHART_SIZE, votes, new_chart);

    print_chart(prev_chart, new_chart);

    add_votes_to_top_votes(top_votes, new_chart);

    add_to_old_hits(prev_chart, new_chart, old_hits);

    prev_chart = new_chart;

    votes.clear();
}

void handle_top(votes_map &top_votes, chart_vector &prev_top_chart) {
    chart_vector new_chart;

    push_best_songs_to_vector(CHART_SIZE, top_votes, new_chart);

    print_chart(prev_top_chart, new_chart);

    prev_top_chart = new_chart;
}

void add_songs_to_votes_map(votes_map &votes, songs_set &songs) {
    for (song_t song: songs) {
        if (votes.find(song) == votes.end()) {
            votes[song] = 0;
        }
        votes[song]++;
    }
}

void print_error(size_t line_num, std::string &line) {
    std::cerr << "Error in line " << line_num << ": " << line << "\n";
}

bool validate_vote(song_t song, unsigned int max, songs_set &old_hits, songs_set &current_songs) {
    if (song < 1 || song > max || song > MAX_SONG
        || old_hits.find(song) != old_hits.end()
        || current_songs.find(song) != current_songs.end()) {
        return false;
    }
    return true;
}

void read_votes_line(unsigned int max, std::string &s, songs_set &old_hits, votes_map &votes, size_t line_counter) {
    unsigned int helper;
    songs_set current_votes;
    std::istringstream my_stream(s);

    while (my_stream) {
        my_stream >> helper;
        if (my_stream) {
            if (!validate_vote(helper, max, old_hits, current_votes)) {
                print_error(line_counter, s);
                return;
            }
            current_votes.insert(helper);
        }
    }
    if (current_votes.empty()) {
        print_error(line_counter, s);
    } else {
        add_songs_to_votes_map(votes, current_votes);
    }
}

void read_new_max(unsigned int &max, std::string &s, votes_map &votes, songs_set &old_hits, chart_vector &prev_chart,
                  votes_map &top_votes, size_t line_counter) {
    std::smatch sm;
    unsigned int max_candidate;

    if (regex_search(s, sm, std::regex("\\d+"))) { // Find any number that exists in the string.
        max_candidate = stoi(sm[0]);
        if (max_candidate >= max && max_candidate > 0 && max_candidate <= MAX_SONG) {
            max = max_candidate;
        } else {
            print_error(line_counter, s);
        }
    }

    handle_new_max(votes, old_hits, prev_chart, top_votes);
}

void read_input(votes_map &votes, votes_map &top_votes,
                songs_set &old_hits, chart_vector &prev_chart, chart_vector &prev_top_chart) {
    size_t line_counter = 1;
    unsigned int max = 0;
    std::string s;

    while (getline(std::cin, s)) {
        if (regex_match(s, vote_regex)) {
            read_votes_line(max, s, old_hits, votes, line_counter);
        } else if (regex_match(s, new_max_regex)) {
            read_new_max(max, s, votes, old_hits, prev_chart, top_votes, line_counter);
        } else if (regex_match(s, top_regex)) {
            handle_top(top_votes, prev_top_chart);
        } else if (!regex_match(s, blank_line_regex)) {
            print_error(line_counter, s);
        }

        line_counter++;
    }
}

int main() {
    votes_map votes;
    votes_map top_votes;
    songs_set old_hits;
    chart_vector prev_chart;
    chart_vector prev_top_chart;

    read_input(votes, top_votes, old_hits, prev_chart, prev_top_chart);

    return 0;
}
