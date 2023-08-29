"""
Copyright 2020 Arizona Board of Regents on behalf of the University of
Arizona

Licensed under the Apache 2 License
"""

import json 
import pandas as pd

def parquet_to_json(path, bb_column = "bounding_box", page_column = "page_num"):
    parquet_df = pd.read_parquet(path)
    parquet_json = parquet_df.to_json()
    parquet_data = json.loads(parquet_json)

    if len(parquet_data) > 0:
        parquet_data_keys = list(parquet_data.keys())
        num_data_rows = max(
            [int(k) for k in parquet_data[parquet_data_keys[0]]]
        )

        row_order_parquet_data = [dict() for i in range(num_data_rows + 1)]
        for field_key, row_data in parquet_data.items():
            for row_idx, datum in row_data.items():
                row_idx_num = int(row_idx)
                row_order_parquet_data[row_idx_num][field_key] = datum

        # if filename == "documents.parquet":
        # Sorts the content sections by page number and then by
        # bounding box location. Use x-pos first to account for
        # multi-column documents and then sort by y-pos.
        row_order_parquet_data.sort(
            key=lambda d: (
                d[page_column],
                d[bb_column][0]
                // 500,  # allows for indentation while still catching items across the center line
                # (d["bounding_box"][0]) // 100
                # + round((d["bounding_box"][0] % 100 // 10) / 10),
                d[bb_column][1],
            )
        )

        edits = list()
        for e1, extraction1 in enumerate(row_order_parquet_data):
            (ext1_x1, ext1_y1, ext1_x2, ext1_y2) = extraction1[bb_column]
            # Don't bother processing for left-justified or centered
            # content ... only right column content needs to be checked
            if ext1_x1 < 500:
                continue

            ext1_page_num = extraction1[page_column]
            found_col_break = False
            insertion_index = -1
            t1 = e1
            while t1 > 0:
                extraction2 = row_order_parquet_data[t1 - 1]
                ext2_page_num = extraction2[page_column]
                # If the previous sorted entry is on an earlier page
                # then we can stop our search
                if ext1_page_num > ext2_page_num:
                    break

                (ext2_x1, ext2_y1, ext2_x2, ext2_y2) = extraction2[bb_column]

                if ext1_y2 <= ext2_y1:
                    ext2_xspan = ext2_x2 - ext2_x1
                    # Useful heuristic cutoff for now
                    if ext2_xspan >= 800:
                        found_col_break = True
                        insertion_index = t1 - 1
                t1 -= 1
            if found_col_break:
                edits.append(
                    {
                        "del_idx": e1,
                        "ins_idx": insertion_index,
                        "val": extraction1,
                    }
                )
        for edit_dict in edits:
            del row_order_parquet_data[edit_dict["del_idx"]]
            row_order_parquet_data.insert(
                edit_dict["ins_idx"], edit_dict["val"]
            )
        row_order_parquet_data.sort(key=lambda d: (d["pdf_name"]))

        name2results = dict()
        for row_data in row_order_parquet_data:
            if row_data["pdf_name"] in name2results:
                name2results[row_data["pdf_name"]].append(row_data)
            else:
                name2results[row_data["pdf_name"]] = [row_data]

        return next(iter(name2results.items()))[1]
