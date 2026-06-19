import docx

doc_path = 'Species_params_word_table.docx'

try:
    doc = docx.Document(doc_path)
    tables = doc.tables
    print(f"Found {len(tables)} tables in the document.")
    
    for i, table in enumerate(tables):
        print(f"\n--- Table {i} ---")
        print(f"Rows: {len(table.rows)}, Columns: {len(table.columns)}")
        if len(table.rows) > 2:
            print("Row 0:", [cell.text.strip().replace('\n', ' ') for cell in table.rows[0].cells])
            print("Row 1:", [cell.text.strip().replace('\n', ' ') for cell in table.rows[1].cells])
            print("Row 2:", [cell.text.strip().replace('\n', ' ') for cell in table.rows[2].cells])
            print("Row 3:", [cell.text.strip().replace('\n', ' ')[:30] for cell in table.rows[3].cells])
            
except Exception as e:
    print(f"Error reading docx: {e}")
