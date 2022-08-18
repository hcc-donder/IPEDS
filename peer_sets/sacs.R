# This script sets up the comparative information 
# manually edit to customize choices

# Set the IPEDS identifier for your institution
my_UNITID <- 218070 # required

#########################################################
# comparison characteristics
#########################################################

# The settngs below are all OPTIONAL
# they each filter schools to create a comparison group
# to DISABLE a filter, set it to NULL, so
# my_STABBR <- NULL will not limit by state
# you can choose multiple values with c(1,2,3), etc

# you can provide a list of UNITIDs of peer schools 
# if you like, or NULL to ignore
# you can find IPEDS IDs in the hd2018 file or the accreditor
# dashboard
my_peers <- NULL # e.g. my_peers <- c(123457,232567) 

# sector
my_SECTOR <- 1:3

#  0	Administrative Unit
#  1	Public, 4-year or above
#  2	Private not-for-profit, 4-year or above
#  3	Private for-profit, 4-year or above
#  4	Public, 2-year
#  5	Private not-for-profit, 2-year
#  6	Private for-profit, 2-year
#  7	Public, less-than 2-year
#  8	Private not-for-profit, less-than 2-year
#  9	Private for-profit, less-than 2-year
#  99	Sector unknown (not active)


# level of institution
my_ICLEVEL <- NULL
# 1	Four or more years
# 2	At least 2 but less than 4 years
# 3	Less than 2 years (below associate)

my_INSTSIZE <- NULL
#  1	Under 1,000
#  2	1,000 - 4,999
#  3	5,000 - 9,999
#  4	10,000 - 19,999
#  5	20,000 and above

# region to include in comparison
my_STABBR <- NULL #  state abbreviations, e.g. c("SC","NC","GA","TN") 

# Carnegie size and residential type
my_C18SZSET <- 6:17
# 1	Two-year, very small
# 2	Two-year, small
# 3	Two-year, medium
# 4	Two-year, large
# 5	Two-year, very large
# 6	Four-year, very small, primarily nonresidential
# 7	Four-year, very small, primarily residential
# 8	Four-year, very small, highly residential
# 9	Four-year, small, primarily nonresidential
# 10	Four-year, small, primarily residential
# 11	Four-year, small, highly residential
# 12	Four-year, medium, primarily nonresidential
# 13	Four-year, medium, primarily residential
# 14	Four-year, medium, highly residential
# 15	Four-year, large, primarily nonresidential
# 16	Four-year, large, primarily residential
# 17	Four-year, large, highly residential
# 18	Exclusively graduate/professional

my_CCBASIC <- NULL
# 1	Associate's--Public Rural-serving Small
# 2	Associate's--Public Rural-serving Medium
# 3	Associate's--Public Rural-serving Large
# 4	Associate's--Public Suburban-serving Single Campus
# 5	Associate's--Public Suburban-serving Multicampus
# 6	Associate's--Public Urban-serving Single Campus
# 7	Associate's--Public Urban-serving Multicampus
# 8	Associate's--Public Special Use
# 9	Associate's--Private Not-for-profit
# 10	Associate's--Private For-profit
# 11	Associate's--Public 2-year colleges under 4-year universities
# 12	Associate's--Public 4-year Primarily Associate's
# 13	Associate's--Private Not-for-profit 4-year Primarily Associate's
# 14	Associate's--Private For-profit 4-year Primarily Associate's
# 15	Research Universities (very high research activity)
# 16	Research Universities (high research activity)
# 17	Doctoral/Research Universities
# 18	Master's Colleges and Universities (larger programs)
# 19	Master's Colleges and Universities (medium programs)
# 20	Master's Colleges and Universities (smaller programs)
# 21	Baccalaureate Colleges--Arts & Sciences
# 22	Baccalaureate Colleges--Diverse Fields
# 23	Baccalaureate/Associate's Colleges
# 24	Theological seminaries, Bible colleges, and other faith-related institutions
# 25	Medical schools and medical centers
# 26	Other health professions schools
# 27	Schools of engineering
# 28	Other technology-related schools
# 29	Schools of business and management
# 30	Schools of art, music, and design
# 31	Schools of law
# 32	Other special-focus institutions
# 33	Tribal Colleges

my_accreditor <- "SACSCC"

# AARTS
# Association of Advanced Rabbinical and Talmudic Schools
# ABA
# American Bar Association
# ABFSE
# American Board of Funeral Service Education Committee on Accreditation
# ABHE
# Association for Bibical Higher Educaiton
# ABHES
# Accrediting Bureau of Health Education Schools
# ACAOM
# Accrediting Commission for Acupuncture and Oriental Medicine
# ACCET
# Accrediting Council for Continuing Education & Training
# ACCSC
# Accrediting Commission of Career Schools and Colleges
# ACICS
# Accrediting Council for Independent Colleges and Schools
# AIJS
# Association of Institutions of Jewish Studies
# AMOA
# American Osteopathic Association
# APMA
# American Podiatric Medical Association
# ATSUSC
# Commission on Accrediting of the Association of Theological Schools
# CANAEP
# Council on Accreditation of Nurse Anesthesia Educational Programs
# COE
# Council on Occupational Education
# COMTA
# Commission on Massage Therapy Accreditation
# DETC
# Distance Education Accrediting Commission
# JRCERT
# Joint Review Committee on Education in Radiologic Technology
# MACTE
# Montessori Accreditation Council for Teacher Education
# MEAC
# Midwifery Education Accreditation Council
# MSACHE
# Middle States Commission on Higher Education
# MSACSS
# Middle States Commission on Secondary Education
# NACCAS
# National Accrediting Commission of Career Arts and Sciences
# NASAD
# National Association of Schools of Arts and Design
# NASD
# National Association of Schools of Dance
# NASM
# National Association of Schools of Music
# NAST
# National Association of Schools of Theatre
# NCACHE
# Higher Learning Commission
# NECHE
# New England Commission on Higher Education
# NLNAC
# Accreditation Commission for Education in Nursing
# NWCCU
# Northwest Commission on Colleges and Universities
# NYBRE
# New York State Board of Regents and the Commissioner of Education
# NYBRVE
# New York State Board of Regents State Education Department Office of the Professions (Public Postsecondary Vocational Education Practical Nursing)
# OKSBVT
# Oklahoma State Board of Career and Technology Education
# PRHROD
# Puerto Rico State Agency for the Approval of Public Postsecondary Vocational and Technical Institutions and Programs
# PSBVE
# Pennsylvania State Board of Vocational Education
# SACSCC
# Southern Association of Colleges and Schools Commission on Colleges
# TRACS
# Transnational Association of Christian Colleges and Schools
# WASCJC
# Western Association of Schools and Colleges Accrediting Commission for Community and Junior Colleges
# WASCSR


