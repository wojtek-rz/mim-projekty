-- Recipes app --

create table recipe (
    id serial primary key,
    title varchar,
    minutes numeric,
    contributor_id serial references user,
    submitted date,
    description varchar,
    interactions numeric,
    calorie_level numeric
);

create table ingredient (
    id serial primary key,
    name varchar,
)
create table tag (
    name varchar primary key,
)

create table recipe_steps (
    recipe_id serial references recipe,
    step varchar,
    constraint recipe_steps_pk primary key (recipe_id, step)
);

create table recipe_tags (
    recipe_id serial references recipe,
    tag_name varchar references tags,
    constraint recipe_steps_pk primary key (recipe_id, tag_name)
);

create table recipe_ingredients (
    recipe_id serial references recipe,
    ingredient_name varchar, -- name can be different from ingredient name, for example "tasty onions", whereas ingredient name is "onion"
    ingredient_id serial references ingredients,
    constraint recipe_steps_pk primary key (recipe_id, ingredient_id)
);

create table user (
    id serial primary key,
    username varchar,
    login varchar,
    password varchar,
)

create table likes (
    who serial references user,
    liked serial references recipe,
)

create table follows (
    follower serial references user,
    followed serial references user,
)

create table admin (
    id serial primary key references user
)