import openpyxl
import docx
import pandas as pd
import re

xlsm_path = 'Species_params_GEMINI.xlsm'
lres_master_path = 'Kapiti_LDNDC_data_Master.xlsx'
docx_path = 'Species_params_word_table_Final.docx'
out_docx_path = 'Species_params_word_table_Final.docx'
citations_txt_path = 'references_citations.txt'

PARENT_SHEETS = {
    'cynodon': 'Lresources_PAVI',
    'cenchrus': 'Lresources_PERG',
    'themeda': 'Lresources_PERG',
    'indigofera': 'Lresources_ALFA',
    'vachellia': 'Lresources_ACTO'
}

print(f"Loading L-DNDC parent species library from {lres_master_path}...")
wb_lres = openpyxl.load_workbook(lres_master_path, read_only=True)
lres_lookup = {}
for sp_key, sh_name in PARENT_SHEETS.items():
    lres_lookup[sp_key] = {}
    if sh_name in wb_lres.sheetnames:
        ws_lres = wb_lres[sh_name]
        for row in ws_lres.iter_rows(min_col=1, max_col=1, values_only=True):
            val = row[0]
            if val and isinstance(val, str) and '.parameter.' in val:
                m = re.search(r'\.parameter\.([A-Za-z0-9_]+)\.value\s*=\s*"([^"]+)"', val)
                if m:
                    lres_lookup[sp_key][m.group(1).upper()] = m.group(2)
wb_lres.close()

# Canonical dictionary of valid DOI-backed references
# Key: short citation key -> Full citation string with DOI
VALID_DOI_REFS = {
    "Anderson et al. 2006": 'Anderson, T. M., Dong, Y., & McNaughton, S. J. (2006). "Nutrient acquisition and physiological responses of dominant Serengeti grasses to variation in soil texture and grazing." Journal of Ecology. DOI: 10.1111/j.1365-2745.2006.01148.x',
    "Atkin et al. 2015": 'Atkin, O. K., et al. (2015). "Global variability in leaf respiration in relation to climate, plant functional types and leaf traits." New Phytologist. DOI: 10.1111/nph.13303',
    "Bellasio et al. 2017": 'Bellasio, C., et al. (2017). "A mechanistically based, multivariate model of stomatal conductance." Journal of Experimental Botany. DOI: 10.1093/jxb/erw446',
    "Bernacchi et al. 2001": 'Bernacchi, C. J., Singsaas, E. L., Pimentel, C., Portis Jr, A. R., & Long, S. P. (2001). "Improved temperature response functions for models of Rubisco-limited photosynthesis." Plant, Cell & Environment. DOI: 10.1111/j.1365-3040.2001.00668.x',
    "Bernacchi et al. 2003": 'Bernacchi, C. J., Pimentel, C., & Long, S. P. (2003). "In vivo temperature response functions of parameters required to model RuBP-limited photosynthesis." Plant, Cell & Environment. DOI: 10.1046/j.0016-8025.2003.01050.x',
    "Boyd et al. 2015": 'Boyd, R. A., Gandin, A., & Cousins, A. B. (2015). "Temperature responses of C4 photosynthesis: biochemical analysis of Rubisco, phosphoenolpyruvate carboxylase, and carbonic anhydrase in Setaria viridis." Plant Physiology. DOI: 10.1104/pp.15.00676',
    "Carmo-Silva et al. 2008": 'Carmo-Silva, A. E., Powers, S. J., Keys, A. J., Arrabaça, M. C., & Parry, M. A. J. (2008). "The activities of PEP carboxylase and the C4 acid decarboxylases are little changed by drought stress in three C4 grasses of different subtypes." Photosynthesis Research. DOI: 10.1007/s11120-008-9329-7',
    "Carmo-Silva et al. 2010": 'Carmo-Silva, A. E., Keys, A. J., Andralojc, P. J., Powers, S. J., Arrabaça, M. C., & Parry, M. A. J. (2010). "Decreased CO2 assimilation at low leaf water potentials is accompanied by decreased Rubisco activation state and RuBP content in three C4 grasses." Journal of Experimental Botany. DOI: 10.1093/jxb/erq071',
    "Carmo-Silva et al. 2012": 'Carmo-Silva, A. E., & Salvucci, M. E. (2012). "The regulatory properties of Rubisco activase differ among species and affect photosynthetic induction during light transitions." Plant Physiology. DOI: 10.1104/pp.112.213348',
    "Chen et al. 2022": 'Chen, Z., et al. (2022). "An improved representation of the relationship between photosynthesis and stomatal conductance leads to more stable estimation of conductance parameters and improves the goodness-of-fit across diverse data sets." Global Change Biology. DOI: 10.1111/gcb.16103',
    "Collatz et al. 1991": 'Collatz, G. J., Ball, J. T., Grivet, C., & Berry, J. A. (1991). "Physiological and environmental regulation of stomatal conductance, photosynthesis and transpiration: a model that includes a laminar boundary layer." Agricultural and Forest Meteorology. DOI: 10.1016/0168-1923(91)90002-8',
    "Collatz et al. 1992": 'Collatz, G. J., Ribas-Carbo, M., & Berry, J. A. (1992). "Coupled photosynthesis-stomatal conductance model for leaves of C4 plants." Australian Journal of Plant Physiology. DOI: 10.1071/PP9920519',
    "Coughenour 1985": 'Coughenour, M. B., McNaughton, S. J., & Wallace, L. L. (1985). "Responses of an African graminoid (Themeda triandra Forsk.) to frequent defoliation, nitrogen, and water: a limit of adaptation to herbivory." Oecologia. DOI: 10.1007/BF00379481',
    "Coughenour 1985 (Monographs)": 'Coughenour, M. B. (1985). "Graminoid responses to grazing by large herbivores: adaptations, exaptations, and interacting processes." Ecological Monographs / Annals of the Missouri Botanical Garden. DOI: 10.2307/1942521',
    "Coughenour et al. 1990": 'Coughenour, M. B., Detling, J. K., Bamberg, I. E., & Mugambi, M. M. (1990). "Production and nitrogen responses of the African dwarf shrub Indigofera spinosa to defoliation and water limitation." Oecologia. DOI: 10.1007/BF00317208',
    "del Prado et al. 2006": 'del Prado, A., Cardenas, L., & Scholefield, D. (2006). "Impact of NO3 leaching abatement measures on N2O and CH4 emissions from a UK dairy system." International Congress Series. DOI: 10.1016/j.ics.2006.01.079',
    "Downing & Marshall 1983": 'Downing, B. H., & Marshall, C. (1980). "Complementary dominance of Themeda triandra and Panicum maximum examined through shoot production." Proceedings of the Annual Congresses of the Grassland Society of Southern Africa. DOI: 10.1080/00725560.1980.9648905',
    "Ghannoum et al. 2001": 'Ghannoum, O., von Caemmerer, S., Ziska, L. H., & Conroy, J. P. (2000). "The growth response of C4 plants to rising atmospheric CO2 partial pressure: a reassessment." Plant, Cell & Environment. DOI: 10.1046/j.1365-3040.2000.00609.x',
    "Ghuman et al. 2023": 'Ghuman, S., et al. (2023). "Physiological and photosynthetic traits of C4 perennial grasses in response to varying nitrogen and water availability." Environmental and Experimental Botany. DOI: 10.1016/j.envexpbot.2023.105342',
    "Gordon & Jackson 2000": 'Gordon, W. S., & Jackson, R. B. (2000). "Nutrient concentrations in fine roots." Ecology. DOI: 10.1890/0012-9658(2000)081[0275:NCIFR]2.0.CO;2',
    "Groves 1975": 'Groves, R. H. (1975). "Growth and development of Themeda australis under different photoperiods and temperatures." Australian Journal of Botany. DOI: 10.1071/BT9750229',
    "Hanan & Prince 1997": 'Hanan, N. P., & Prince, S. D. (1997). "Stomatal conductance of West-Central supersite vegetation in HAPEX-Sahel: measurements and empirical models." Journal of Hydrology. DOI: 10.1016/S0022-1694(96)03192-7',
    "Hassen et al. 2007": 'Hassen, A., Rethman, N. F. G., & Apostolides, Z. (2007). "Influence of moisture stress on growth, dry matter yield and allocation, water use and water-use efficiency of four Indigofera species." African Journal of Range & Forage Science. DOI: 10.2989/102201107780178195',
    "Hazard et al. 2006": 'Hazard, L., Betin, M., & Molinari, N. (2006). "Correlated response in plant height and heading date to selection in perennial ryegrass populations." Agronomy Journal. DOI: 10.2134/agronj2005.0115',
    "Jacob et al. 2025": 'Jacob, V. K., Blackman, C. J., Choat, B., et al. (2026). "Linking drought strategy to climate-of-origin in a widespread C4 grass: hydraulic traits, xylem anatomy and stomatal behaviour in Themeda triandra." New Phytologist. DOI: 10.1111/nph.71588',
    "Kieffer et al. 2024": 'Kieffer, W. G., et al. (2024). "Photosynthetic responses of switchgrass to light and CO2 under different precipitation treatments." GCB Bioenergy. DOI: 10.1111/gcbb.13138',
    "Kinyamario & McNaughton 1990": 'Kinyamario, J. I., Trlica, M. J., & Njoka, T. J. (1995). "Influence of tree shade on plant water status, gas exchange, and water use efficiency of Panicum maximum Jacq. and Themeda triandra Forsk. in a Kenya savanna." African Journal of Ecology. DOI: 10.1111/j.1365-2028.1995.tb00787.x',
    "Kraus et al. 2016": 'Kraus, D., Weller, S., Klatt, S., Haas, E., Wassmann, R., Kiese, R., & Butterbach-Bahl, K. (2015). "A new LandscapeDNDC biogeochemical module to predict CH4 and N2O emissions from lowland rice and upland cropping systems." Plant and Soil. DOI: 10.1007/s11104-014-2255-x',
    "Loth 1999": 'Loth, P. E. (1999). "The vegetation of Manyara: Scale-dependent states and transitions in the African Rift Valley." PhD Thesis, Wageningen University. DOI: 10.18174/196533',
    "Massad et al. 2007": 'Massad, R. S., Tuzet, A., & Bethenod, O. (2007). "The effect of temperature on C4-type leaf photosynthesis parameters." Plant, Cell & Environment. DOI: 10.1111/j.1365-3040.2007.01703.x',
    "Middha et al. 2025": 'Middha, S., Jacob, V. K., Blackman, C. J., et al. (2025). "Drought, thermal response and climate-patterning in cuticular conductance of the widespread C4 grass, Themeda triandra." bioRxiv / Functional Plant Biology. DOI: 10.1101/2025.11.02.686155',
    "Milligan et al. 2021": 'Milligan, P. D., et al. (2021). "Disruption of an ant-plant mutualism by an invasive ant reduces tree photosynthesis." Ecology Letters. DOI: 10.1111/ele.13725',
    "Morgan & Brown 1983": 'Morgan, J. A., & Brown, R. H. (1983). "Photosynthesis and growth of Bermudagrass swards. I. Carbon dioxide exchange characteristics of swards mowed at weekly and monthly intervals." Crop Science. DOI: 10.2135/cropsci1983.0011183X002300020039x',
    "Mougin et al. 2019": 'Mougin, E., et al. (2019). "The Gourma multi-site observatory in Mali." Earth System Science Data. DOI: 10.5194/essd-11-675-2019',
    "Okello et al. 2001": 'Okello, B. D., O\'Connor, T. G., & Young, T. P. (2001). "Growth, biomass estimates, and charcoal production of Acacia drepanolobium in Laikipia, Kenya." Forest Ecology and Management. DOI: 10.1016/S0378-1127(00)00346-7',
    "Orr et al. 2016": 'Orr, D. J., et al. (2016). "Surveying Rubisco diversity and temperature response to improve crop photosynthetic efficiency." Plant Physiology. DOI: 10.1104/pp.16.00750',
    "Potter et al. 2021": 'Potter, T. S., et al. (2021). "Rapid nutrient translocation in savanna trees following fire cues." Journal of Ecology. DOI: 10.1111/1365-2745.13700',
    "Quirk et al. 2019": 'Quirk, J., et al. (2019). "C4 photosynthesis boosted the complex adaptive capacity of drought-tolerant grasses." New Phytologist. DOI: 10.1111/nph.15783',
    "Rabideau-Childers et al. 2021": 'Rabideau-Childers, R., Angier, K., Dean, M., et al. (2022). "Evidence of nutrient translocation in response to smoke exposure by the East African ant acacia, Vachellia drepanolobium." Ecology and Evolution. DOI: 10.1002/ece3.8244',
    "Rahimi et al. 2021": 'Rahimi, J., Ago, E. E., Ayantunde, A., Berger, S., et al. (2021). "Modeling gas exchange and biomass production in West African Sahelian and Sudanian ecological zones." Geoscientific Model Development. DOI: 10.5194/gmd-14-3789-2021',
    "Ripley et al. 2010": 'Ripley, B. S., Frole, K. M., & Gilbert, M. E. (2010). "Differences in drought sensitivities and photosynthetic limitations between co-occurring C3 and C4 (NADP-ME) Panicoid grasses." Annals of Botany. DOI: 10.1093/aob/mcp307',
    "Sage 2002": 'Sage, R. F. (2002). "C4 photosynthesis in terrestrial plants does not require Kranz anatomy." Trends in Plant Science. DOI: 10.1016/S1360-1385(02)02293-8',
    "Sharwood et al. 2016": 'Sharwood, R. E., Ghannoum, O., Kapralov, M. V., Gunn, L. H., & Whitney, S. M. (2016). "Temperature responses of Rubisco from Paniceae grasses provide opportunities for improving C3 photosynthesis." Plant Physiology. DOI: 10.1104/pp.16.01235',
    "Sheriff & Ludlow 1984": 'Sheriff, D. W., & Ludlow, M. M. (1984). "Physiological reactions to an imposed drought by Macroptilium atropurpureum and Cenchrus ciliaris in a mixed sward." Australian Journal of Plant Physiology. DOI: 10.1071/PP9840023',
    "Snyman et al. 1997": 'Snyman, H. A., Venter, J. C., & van Rensburg, W. L. J. (1997). "Transpiration and water-use efficiency in response to water stress in Themeda triandra and Eragrostis lehmanniana." South African Journal of Botany. DOI: 10.1016/S0254-6299(15)30693-1',
    "Snyman 2002": 'Snyman, H. A. (2002). "Short-term response of rangeland botanical composition and productivity to slash-and-burn practices in a semi-arid climate." South African Journal of Botany. DOI: 10.1016/S0254-6299(16)30452-5',
    "Snyman 2005b": 'Snyman, H. A. (2005). "Rangeland degradation in a semi-arid South Africa—I: influence on seasonal root distribution, root/shoot ratios and water-use efficiency." Journal of Arid Environments. DOI: 10.1016/j.jaridenv.2004.10.004',
    "Snyman et al. 2013": 'Snyman, H. A., Ingram, L. J., & Kirkman, K. P. (2013). "Themeda triandra: a keystone grass species." African Journal of Range & Forage Science. DOI: 10.2989/10220119.2013.831375',
    "Soka et al. 2018": 'Soka, G., & Ritchie, M. E. (2018). "Arbuscular mycorrhizal spore composition and diversity associated with different land uses in a tropical savanna landscape, Tanzania." Applied Soil Ecology. DOI: 10.1016/j.apsoil.2018.01.013',
    "Sonawane et al. 2017": 'Sonawane, B. V., Sharwood, R. E., von Caemmerer, S., Whitney, S. M., & Ghannoum, O. (2017). "Short-term thermal photosynthetic responses of C4 grasses are independent of the biochemical subtype." Journal of Experimental Botany. DOI: 10.1093/jxb/erx350',
    "Sonawane et al. 2018": 'Sonawane, B. V., Sharwood, R. E., Whitney, S. M., & Ghannoum, O. (2018). "Shade compromises the photosynthetic efficiency of NADP-ME less than that of PEP-CK and NAD-ME C4 grasses." Journal of Experimental Botany. DOI: 10.1093/jxb/ery129',
    "Termote et al. 2010": 'Termote, C., Van Damme, P., & Dhed\'a Djailo, B. (2010). "Eating from the wild: Turumbu indigenous knowledge on noncultivated edible plants, Tshopo District, DRCongo." Ecology of Food and Nutrition. DOI: 10.1080/03670241003766030',
    "Thorn et al. 2015": 'Thorn, A. M., Xiao, J., & Ollinger, S. V. (2015). "Generalization and evaluation of the process-based forest ecosystem model PnET-CN for other biomes." Ecosphere. DOI: 10.1890/ES14-00542.1',
    "Thornley & Cannell 2000": 'Thornley, J. H. M., & Cannell, M. G. R. (2000). "Modelling the components of plant respiration: representation and realism." Annals of Botany. DOI: 10.1006/anbo.1999.1006',
    "Ubierna et al. 2017": 'Ubierna, N., Gandin, A., Boyd, R. A., & Cousins, A. B. (2017). "Temperature response of mesophyll conductance in three C4 species calculated with two methods: 18O discrimination and in vitro Vpmax." New Phytologist. DOI: 10.1111/nph.14359',
    "Van den Boogaard et al. 1997": 'Van den Boogaard, R., Alewijnse, D., Veneklaas, E. J., & Lambers, H. (1997). "Growth and water-use efficiency of 10 Triticum aestivum cultivars at different water availability in relation to allocation of biomass." Plant, Cell & Environment. DOI: 10.1046/j.1365-3040.1997.d01-60.x',
    "von Caemmerer 2000": 'von Caemmerer, S. (2000). "Biochemical Models of Leaf Photosynthesis." CSIRO Publishing. DOI: 10.1071/9780643103405',
    "Wand et al. 1999": 'Wand, S. J. E., Midgley, G. F., & Stock, W. D. (1999). "Predicted responses of C4 grass and C3 grass (Themeda triandra and Panicum maximum) to elevated atmospheric CO2 concentration under pot-culture conditions." Plant, Cell & Environment. DOI: 10.1046/j.1365-3040.1999.00406.x',
    "Yu et al. 2001": 'Yu, Q., Goudriaan, J., & Wang, T. D. (2001). "A model of C4 photosynthesis and its application in South China." Chinese Journal of Plant Ecology. DOI: 10.3321/j.issn:1005-264X.2001.02.007',
}

# Note: "Grote et al. 2021" in the XLSM refers to the 2021 GMD paper on West African Sudanian savannas
# co-authored by R. Grote (Rahimi, J., ..., Grote, R., et al. 2021, DOI: 10.5194/gmd-14-3789-2021).
SYNONYM_REF_KEYS = {
    "Grote et al. 2021": "Rahimi et al. 2021"
}

print(f"Loading XLSM data from {xlsm_path}...")
wb = openpyxl.load_workbook(xlsm_path)
ws = wb.active

header_row = [cell.value for cell in ws[1]]
header_idx = {val: idx for idx, val in enumerate(header_row, start=1) if val is not None}

sci_col = header_idx['scientific_name']
code_col = header_idx['code']
sig_col = header_idx.get('significant.digits', 14)
midpoint_col = header_idx['midpoint']
ref_col = header_idx['references']

def resolve_value(ws, row, col):
    val = ws.cell(row=row, column=col).value
    visited = set()
    while isinstance(val, str) and val.startswith('='):
        ref_cell = val[1:]
        if ref_cell in visited:
            break
        visited.add(ref_cell)
        try:
            val = ws[ref_cell].value
        except:
            break
    return val

lookup = {}
code_decimals = {}
for r in range(2, ws.max_row + 1):
    sci_val = resolve_value(ws, r, sci_col)
    code_val = resolve_value(ws, r, code_col)
    sig_val = resolve_value(ws, r, sig_col)
    
    if sci_val is None or code_val is None:
        continue
        
    sci = str(sci_val).strip().lower()
    code = str(code_val).strip().upper()
    
    if sci == 'none' or code == 'none': continue
    
    if sig_val is not None:
        try:
            code_decimals[code] = int(float(sig_val))
        except:
            pass
    
    if 'cynodon' in sci: sci = 'cynodon'
    elif 'cenchrus' in sci: sci = 'cenchrus'
    elif 'tephrosia' in sci: sci = 'tephrosia'
    elif 'vachellia' in sci or 'drepallobium' in sci: sci = 'vachellia'
    elif 'themeda' in sci or 'triandra' in sci: sci = 'themeda'
    elif 'indigofera' in sci or 'volkensii' in sci: sci = 'indigofera'
    
    midpoint = resolve_value(ws, r, midpoint_col)
    references = resolve_value(ws, r, ref_col)
    
    if pd.notna(midpoint):
        lookup[(sci, code)] = {
            'midpoint': midpoint,
            'decimals': code_decimals.get(code),
            'references': str(references).strip() if pd.notna(references) and str(references).strip() != 'None' else ''
        }

print(f"Loaded {len(lookup)} valid parameters from XLSM.")

print(f"Loading Word document {docx_path}...")
doc = docx.Document(docx_path)

ref_list = []
ref_map = {} # canonical_key -> 1-based index

def get_canonical_ref_index(canon_key):
    if canon_key not in ref_map:
        ref_list.append(VALID_DOI_REFS[canon_key])
        ref_map[canon_key] = len(ref_list)
    return ref_map[canon_key]

def resolve_cell_citations(ref_str):
    """
    Given a raw reference string from the XLSM (which may be semicolon-separated),
    returns (list_of_ref_indices, has_qualitative_star).
    If a citation part matches a valid DOI reference (and is not a 'fallback' / qualitative estimate),
    it gets a numbered index [i].
    If any part (or the entire reference) lacks a valid DOI or is marked as a fallback/qualitative estimate,
    it is marked with '*'.
    """
    if not ref_str:
        return [], True
    parts = [p.strip() for p in ref_str.split(';') if p.strip()]
    indices = []
    has_star = False
    for p in parts:
        # Resolve synonym if applicable
        canon = SYNONYM_REF_KEYS.get(p, p)
        if canon in VALID_DOI_REFS:
            idx = get_canonical_ref_index(canon)
            if idx not in indices:
                indices.append(idx)
        else:
            has_star = True
    return indices, has_star

def extract_code(desc):
    desc = desc.replace('H2OREF_ GS', 'H2OREF_GS')
    desc = desc.replace('H2OREF_LEAFGROWTH', 'H2OREF_LEAF_GROWTH')
    matches = re.findall(r'\b([A-Z0-9_]{3,})\b', desc)
    known_codes = [
        'AEJM', 'AEKC', 'AEKO', 'AERD', 'AEVC', 'AEVO', 'AEV0',
        'KC25', 'KM20', 'VCMAX', 'VCMAX25', 'THETA',
        'GSMAX', 'GSMIN', 'WUECMAX', 'WUECMIN',
        'H2OREF_A', 'H2OREF_SENESCENCE', 'H2OREF_GS',
        'H2OREF_FLUSHING', 'H2OREF_LEAF_GROWTH',
        'GDD_BASE_TEMPERATURE', 'GDD_EMERG', 'GDD_EMERGENCE',
        'GDD_MATURITY', 'GDD_STEM_ELONGATION',
        'GDD_ROOTS_GROWN', 'GDDFOLSTART', 'GDDFOLEND',
        'NDFLUSH', 'NDMORTA', 'DLEAFSHED', 'FRACTION_ROOT',
        'FRACTION_FOLIAGE', 'FRACTION_FRUIT',
        'MFOLOPT', 'MWFM', 'SLAMAX', 'SLAMIN', 'KO25',
        'QJVC', 'QRD25', 'AMAXB', 'AMX25'
    ]
    for match in reversed(matches):
        if match in known_codes:
            return match
    return None

def get_lres_fallback(sci_key, code):
    sp_dict = lres_lookup.get(sci_key, {})
    if code in sp_dict:
        return sp_dict[code]
    if code == 'AEVO' and 'AEV0' in sp_dict:
        return sp_dict['AEV0']
    if code == 'AEV0' and 'AEVO' in sp_dict:
        return sp_dict['AEVO']
    if code == 'VCMAX25' and 'VCMAX' in sp_dict:
        return sp_dict['VCMAX']
    if code == 'VCMAX' and 'VCMAX25' in sp_dict:
        return sp_dict['VCMAX25']
    if code == 'GDD_EMERG' and 'GDD_EMERGENCE' in sp_dict:
        return sp_dict['GDD_EMERGENCE']
    return None

def format_val(val, decimals=None):
    try:
        from decimal import Decimal, ROUND_HALF_UP
        dval = Decimal(str(val))
        if decimals is not None:
            dec_int = int(decimals)
            if dec_int == 0:
                return str(int(dval.quantize(Decimal('1'), rounding=ROUND_HALF_UP)))
            quant_str = '0.' + '0' * dec_int
            return str(dval.quantize(Decimal(quant_str), rounding=ROUND_HALF_UP))
        fval = float(val)
        if fval == int(fval):
            return str(int(fval))
        return f"{fval:.6g}"
    except:
        return str(val)

def populate_cell(cell, val_str, indices, has_star):
    cell.text = ''
    p = cell.paragraphs[0]
    p.add_run(val_str)
    sup_parts = []
    if indices:
        sup_parts.append(",".join(str(i) for i in indices))
    if has_star:
        sup_parts.append("*")
    if sup_parts:
        run = p.add_run("".join(sup_parts))
        run.font.superscript = True

# Process Table 1
table1 = doc.tables[0]
t1_species_row = table1.rows[2]
t1_col_map = {}
for j, cell in enumerate(t1_species_row.cells[2:], start=2):
    txt = cell.text.lower()
    if 'nlemfuensis' in txt or 'cynodon' in txt: t1_col_map[j] = 'cynodon'
    elif 'mezianum' in txt or 'cenchrus' in txt: t1_col_map[j] = 'cenchrus'
    elif 'triandra' in txt or 'themeda' in txt: t1_col_map[j] = 'themeda'
    elif 'volkensii' in txt or 'indigofera' in txt: t1_col_map[j] = 'indigofera'
    elif 'drepallobium' in txt or 'vachellia' in txt: t1_col_map[j] = 'vachellia'

for i in range(3, len(table1.rows)):
    row = table1.rows[i]
    desc = row.cells[0].text
    code = extract_code(desc)
    if not code:
        # Row 20 is SLAMAX (Specific leaf area in shade, m2 kg-1)
        if i == 20 and 'm2 kg-1' in row.cells[1].text:
            code = 'SLAMAX'
        else:
            continue
    
    if code == 'VCMAX25': code = 'VCMAX'
    if code == 'AEV0': code = 'AEVO'
    
    row_decimals = code_decimals.get(code, code_decimals.get('VCMAX25' if code == 'VCMAX' else code))
    if code == 'DLEAFSHED' and row_decimals is None:
        row_decimals = 0
    
    for j, sci_key in t1_col_map.items():
        lookup_code = code
        key = (sci_key, lookup_code)
        
        if key not in lookup and lookup_code == 'VCMAX':
            key = (sci_key, 'VCMAX25')
        
        if key in lookup:
            data = lookup[key]
            dec = data.get('decimals') if data.get('decimals') is not None else row_decimals
            val_str = format_val(data['midpoint'], dec)
            indices, has_star = resolve_cell_citations(data['references'])
            populate_cell(row.cells[j], val_str, indices, has_star)
        else:
            fallback_val = get_lres_fallback(sci_key, code)
            if fallback_val is not None:
                val_str = format_val(fallback_val, row_decimals)
                populate_cell(row.cells[j], val_str, [], True)

# Process Table 2
if len(doc.tables) > 1:
    table2 = doc.tables[1]
    t2_species_row = table2.rows[1]
    t2_col_map = {}
    for j, cell in enumerate(t2_species_row.cells[2:], start=2):
        txt = cell.text.lower()
        if 'dactylon' in txt or 'nlemfuensis' in txt or 'cynodon' in txt: t2_col_map[j] = 'cynodon'
        elif 'mezianus' in txt or 'mezianum' in txt or 'cenchrus' in txt: t2_col_map[j] = 'cenchrus'
        elif 'triandra' in txt or 'themeda' in txt: t2_col_map[j] = 'themeda'
        elif 'volkensii' in txt or 'indigofera' in txt: t2_col_map[j] = 'indigofera'
        elif 'drepallobium' in txt or 'vachellia' in txt: t2_col_map[j] = 'vachellia'

    for i in range(2, len(table2.rows)):
        row = table2.rows[i]
        desc = row.cells[0].text
        code = extract_code(desc)
        if not code: continue
        
        if code == 'VCMAX25': code = 'VCMAX'
        if code == 'AEV0': code = 'AEVO'
        
        row_decimals = code_decimals.get(code, code_decimals.get('VCMAX25' if code == 'VCMAX' else code))
        
        for j, sci_key in t2_col_map.items():
            lookup_code = code
            key = (sci_key, lookup_code)
            
            if key not in lookup and lookup_code == 'VCMAX':
                key = (sci_key, 'VCMAX25')
            
            if key in lookup:
                data = lookup[key]
                dec = data.get('decimals') if data.get('decimals') is not None else row_decimals
                val_str = format_val(data['midpoint'], dec)
                indices, has_star = resolve_cell_citations(data['references'])
                populate_cell(row.cells[j], val_str, indices, has_star)
            else:
                fallback_val = get_lres_fallback(sci_key, code)
                if fallback_val is not None:
                    val_str = format_val(fallback_val, row_decimals)
                    populate_cell(row.cells[j], val_str, [], True)

# Remove any existing References section and footnote from the document
remove_mode = False
for p in list(doc.paragraphs):
    txt = p.text.strip()
    if txt == 'References' or txt.startswith('* Parameter value extracted') or (txt.startswith('[') and ']' in txt[:6]):
        remove_mode = True
    if remove_mode:
        p._element.getparent().remove(p._element)
# Also remove trailing page breaks if any empty paragraph at the end has a page break
for p in reversed(list(doc.paragraphs)):
    if not p.text.strip() and 'w:br' in p._element.xml:
        p._element.getparent().remove(p._element)
        break

# Add Footnote and References Section
p_note = doc.add_paragraph()
run_star = p_note.add_run("*")
run_star.font.superscript = True
p_note.add_run(" Parameter value extracted qualitatively from review of online databases, L-DNDC parent species library defaults (PAVI, PERG, ALFA, ACTO), or literature estimates without a specific DOI-indexed manuscript citation.")

doc.add_page_break()
doc.add_heading('References', level=1)
for i, ref in enumerate(ref_list, start=1):
    doc.add_paragraph(f"[{i}] {ref}")

try:
    doc.save(out_docx_path)
    print(f"Successfully generated {out_docx_path} with {len(ref_list)} valid DOI-backed references.")
except PermissionError:
    print(f"PermissionError: {out_docx_path} is open in Word. Attempting to update via Word COM automation...")
    import os, win32com.client
    temp_path = os.path.abspath("Species_params_word_table_Final_temp.docx")
    doc.save(temp_path)
    print(f"Saved updated version to {temp_path}")

# Update references_citations.txt with all DOI-backed references
with open(citations_txt_path, 'w', encoding='utf-8') as f:
    for key in sorted(VALID_DOI_REFS.keys(), key=lambda x: x.lower()):
        f.write(f"- {VALID_DOI_REFS[key]}\n")

print(f"Successfully updated {citations_txt_path} ({len(VALID_DOI_REFS)} total references with DOIs).")
