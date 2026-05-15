#include <stdio.h>
#include <stdlib.h>
#include <sqlite3.h>
#include <time.h>


void
init_db(
    sqlite3 *db
) {
    const char *sql =
        "CREATE TABLE IF NOT EXISTS or_set ("
        "  element TEXT NOT NULL,"
        "  tag TEXT NOT NULL,"
        "  added_at INTEGER NOT NULL,"
        "  removed_at INTEGER,"
        "  PRIMARY KEY (element, tag)"
        ");";

    char *err = NULL;
    if (sqlite3_exec(db, sql, 0, 0, &err) != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", err);
        sqlite3_free(err);
        exit(1);
    }
}


long
current_time_ms() {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}


void
or_set_add(
    sqlite3 *db,
    const char *element,
    const char *tag
) {
    const char *sql = "INSERT OR IGNORE INTO or_set(element, tag, added_at) VALUES (?, ?, ?);";
    sqlite3_stmt *stmt;

    sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    sqlite3_bind_text(stmt, 1, element, -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 2, tag, -1, SQLITE_STATIC);
    sqlite3_bind_int64(stmt, 3, current_time_ms());
    sqlite3_step(stmt);

    sqlite3_finalize(stmt);
}


void
or_set_remove(
    sqlite3 *db,
    const char *element
) {
    const char *sql = "UPDATE or_set SET removed_at = ? WHERE element = ? AND removed_at IS NULL;";
    sqlite3_stmt *stmt;

    sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    sqlite3_bind_int64(stmt, 1, current_time_ms());
    sqlite3_bind_text(stmt, 2, element, -1, SQLITE_STATIC);
    sqlite3_step(stmt);

    sqlite3_finalize(stmt);
}


void
or_set_print(
    sqlite3 *db
) {
    const char *sql = "SELECT element, MAX(added_at), MAX(removed_at) FROM or_set GROUP BY element;";
    sqlite3_stmt *stmt;

    sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);

    printf("Current OR-Set:\n");

    while (sqlite3_step(stmt) == SQLITE_ROW) {
        const unsigned char *element = sqlite3_column_text(stmt, 0);

        long added = sqlite3_column_int64(stmt, 1);
        long removed = sqlite3_column_type(stmt, 2) == SQLITE_NULL ? 0 : sqlite3_column_int64(stmt, 2);

        if (added > removed) {
            printf(" - %s\n", element);
        }
    }
    sqlite3_finalize(stmt);
}


void
or_set_merge(
    sqlite3 *db,
    sqlite3 *other
) {
    const char *sql = "SELECT element, tag, added_at, removed_at FROM or_set;";
    sqlite3_stmt *stmt;

    sqlite3_prepare_v2(other, sql, -1, &stmt, NULL);

    while (sqlite3_step(stmt) == SQLITE_ROW) {
        const char *element = (const char *)sqlite3_column_text(stmt, 0);
        const char *tag = (const char *)sqlite3_column_text(stmt, 1);

        long added_at = sqlite3_column_int64(stmt, 2);
        long removed_at = sqlite3_column_type(stmt, 3) == SQLITE_NULL ? 0 : sqlite3_column_int64(stmt, 3);

        const char *insert_sql =
            "INSERT OR IGNORE INTO or_set(element, tag, added_at, removed_at) VALUES (?, ?, ?, ?);";

        sqlite3_stmt *ins;

        sqlite3_prepare_v2(db, insert_sql, -1, &ins, NULL);
        sqlite3_bind_text(ins, 1, element, -1, SQLITE_STATIC);
        sqlite3_bind_text(ins, 2, tag, -1, SQLITE_STATIC);
        sqlite3_bind_int64(ins, 3, added_at);

        if (removed_at) sqlite3_bind_int64(ins, 4, removed_at);
        else sqlite3_bind_null(ins, 4);

        sqlite3_step(ins);
        sqlite3_finalize(ins);
    }

    sqlite3_finalize(stmt);
}

int main() {
    sqlite3 *db1, *db2;
    sqlite3_open("replica1.db", &db1);
    sqlite3_open("replica2.db", &db2);

    init_db(db1);
    init_db(db2);

    or_set_add(db1, "a", "ka");
    or_set_add(db2, "b", "kha");

    or_set_remove(db1, "a");

    printf("before merge:\n");

    printf("replica1:\n");
    or_set_print(db1);

    printf("replica2:\n");
    or_set_print(db2);

    or_set_merge(db1, db2);
    or_set_merge(db2, db1);

    printf("\nafter merge:\n");
    printf("replica1:\n");
    or_set_print(db1);
    printf("replica2:\n");
    or_set_print(db2);

    sqlite3_close(db1);
    sqlite3_close(db2);

    return 0;
}
