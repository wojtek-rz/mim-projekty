//
// Created by Jakub Kłos and Wojciech Rzepliński
//

#ifndef ORGANISM_H
#define ORGANISM_H

#include <cstdint>
#include <optional>
#include <concepts>
#include<tuple>
#include<iostream>

template<typename species_t, bool can_eat_meat, bool can_eat_plants> requires std::equality_comparable<species_t>
class Organism {
public:

    constexpr Organism(species_t const &species, uint64_t vitality) :
            species(species),
            vitality(vitality) {};

    [[nodiscard]] inline constexpr uint64_t get_vitality() const { return vitality; }

    [[nodiscard]] inline constexpr const species_t &get_species() const { return species; }

    [[nodiscard]] inline constexpr bool is_dead() const { return vitality == 0; }

private:
    species_t species;
    uint64_t vitality;
};

template<typename species_t>
using Carnivore = Organism<species_t, true, false>;

template<typename species_t>
using Omnivore = Organism<species_t, true, true>;

template<typename species_t>
using Herbivore = Organism<species_t, false, true>;

template<typename species_t>
using Plant = Organism<species_t, false, false>;

// Both can eat each other.
template<typename species_t, bool sp1_eats_p, bool sp2_eats_p>
constexpr std::tuple<Organism<species_t, true, sp1_eats_p>,
        Organism<species_t, true, sp2_eats_p>,
        std::optional<Organism<species_t, true, sp1_eats_p>>>
encounter(Organism<species_t, true, sp1_eats_p> const &organism1,
          Organism<species_t, true, sp2_eats_p> const &organism2) {
    uint64_t vit1 = organism1.get_vitality(), vit2 = organism2.get_vitality();
    species_t sp1 = organism1.get_species(), sp2 = organism2.get_species();

    if (sp1 == sp2 && sp1_eats_p == sp2_eats_p) {
        return std::make_tuple(Organism<species_t, true, sp1_eats_p>(sp1, vit1),
                               Organism<species_t, true, sp2_eats_p>(sp2, vit2),
                               std::make_optional(Organism<species_t, true, sp1_eats_p>(sp2, (vit1 + vit2) / 2)));
    }

    if (vit1 > vit2) {
        return std::make_tuple(Organism<species_t, true, sp1_eats_p>(sp1, vit1 + vit2 / 2),
                               Organism<species_t, true, sp2_eats_p>(sp2, 0),
                               std::nullopt);
    } else if (vit1 < vit2) {
        return std::make_tuple(Organism<species_t, true, sp1_eats_p>(sp1, 0),
                               Organism<species_t, true, sp2_eats_p>(sp2, vit2 + vit1 / 2),
                               std::nullopt);
    } else {
        return std::make_tuple(Organism<species_t, true, sp1_eats_p>(sp1, 0),
                               Organism<species_t, true, sp2_eats_p>(sp2, 0),
                               std::nullopt);;
    }
}

// Herbivorous is eaten, version 1.
template<typename species_t, bool sp1_eats_p>
constexpr std::tuple<Organism<species_t, true, sp1_eats_p>, Herbivore<species_t>, std::optional<Organism<species_t, true, sp1_eats_p>>>
encounter(Organism<species_t, true, sp1_eats_p> const &predator,
          Herbivore<species_t> const &herbivore) {
    if (predator.get_vitality() > herbivore.get_vitality()) {
        return std::make_tuple(
                Organism<species_t, true, sp1_eats_p>(predator.get_species(),
                                                      predator.get_vitality() + herbivore.get_vitality() / 2),
                Herbivore<species_t>(herbivore.get_species(), 0), std::nullopt);
    } else {
        return std::make_tuple(Organism(predator), Organism(herbivore), std::nullopt);
    }
}

// Herbivorous is eaten, version 2.
template<typename species_t, bool sp2_eats_p>
constexpr std::tuple<Herbivore<species_t>, Organism<species_t, true, sp2_eats_p>, std::optional<Herbivore<species_t>>>
encounter(Herbivore<species_t> const &herbivore,
          Organism<species_t, true, sp2_eats_p> const &predator) {
    if (predator.get_vitality() > herbivore.get_vitality()) {
        return std::make_tuple(
                Herbivore<species_t>(herbivore.get_species(), 0),
                Organism<species_t, true, sp2_eats_p>(predator.get_species(),
                                                      predator.get_vitality() + herbivore.get_vitality() / 2),
                std::nullopt);
    } else {
        return std::make_tuple(Organism(herbivore), Organism(predator), std::nullopt);
    }
}


// Plant is eaten, version 1.
template<typename species_t, bool eats_m>
constexpr std::tuple<Organism<species_t, eats_m, true>, Plant<species_t>, std::optional<Organism<species_t, eats_m, true>>>
encounter(Organism<species_t, eats_m, true> const &organism,
          Plant<species_t> const &plant) {
    return std::make_tuple(Organism<species_t, eats_m, true>(organism.get_species(),
                                                             organism.get_vitality() + plant.get_vitality()),
                           Plant<species_t>(plant.get_species(), 0),
                           std::nullopt);
}

// Plant is eaten, version 2.
template<typename species_t, bool eats_m>
constexpr std::tuple<Plant<species_t>, Organism<species_t, eats_m, true>, std::optional<Plant<species_t>>>
encounter(Plant<species_t> const &plant,
          Organism<species_t, eats_m, true> const &organism) {
    return std::make_tuple(Plant<species_t>(plant.get_species(), 0),
                           Organism<species_t, eats_m, true>(organism.get_species(),
                                                             organism.get_vitality() + plant.get_vitality()),
                           std::nullopt);
}

// Plant can't be eaten by the carnivore, version 1.
template<typename species_t>
constexpr std::tuple<Carnivore<species_t>, Plant<species_t>, std::optional<Carnivore<species_t>>>
encounter(Carnivore<species_t> const &organism1, Plant<species_t> const &organism2) {
    return std::make_tuple(Organism(organism1), Organism(organism2), std::nullopt);
}

// Plant can't be eaten by the carnivore, version 2.
template<typename species_t>
constexpr std::tuple<Plant<species_t>, Carnivore<species_t>, std::optional<Plant<species_t>>>
encounter(Plant<species_t> const &organism1, Carnivore<species_t> const &organism2) {
    return std::make_tuple(Organism(organism1), Organism(organism2), std::nullopt);
}

// Two herbivores meet.
template<typename species_t>
constexpr std::tuple<Herbivore<species_t>,
        Herbivore<species_t>,
        std::optional<Herbivore<species_t>>>
encounter(Herbivore<species_t> const &organism1, Herbivore<species_t> const &organism2) {
    if (organism1.get_species() == organism2.get_species()) {
        return std::make_tuple(Organism(organism1), Organism(organism2),
                               std::make_optional(Herbivore<species_t>(organism1.get_species(),
                                                                       (organism1.get_vitality() +
                                                                        organism2.get_vitality()) / 2)));
    }
    return std::make_tuple(Organism(organism1), Organism(organism2), std::nullopt);
}

template<typename species_t, bool sp1_eats_m, bool sp1_eats_p, typename ... Args>
constexpr Organism<species_t, sp1_eats_m, sp1_eats_p>
encounter_series(Organism<species_t, sp1_eats_m, sp1_eats_p> organism1, Args ... args) {
    ([&] {
        organism1 = get<0>(encounter(organism1, args));
    }(), ...);

    return organism1;
}

#define ZADANIE4_ORGANISM_H

#endif //ZADANIE4_ORGANISM_H
