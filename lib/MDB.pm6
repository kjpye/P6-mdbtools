unit module MDB;

use NativeCall;

# MDB Tools - A library for reading MS Access database files
# Copyright (C) 2000 Brian Bruns
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
# Converted to Perl6 by Kevin Pye, (C) 2019

class GPtrArray is repr('CStruct') { ### FIX
  has uint8 $a;
}

constant MDB-DEBUG         = 0;

constant MDB-PGSIZE        = 4096;
# //#define MDB_MAX_OBJ_NAME (256*3) /* unicode 16 -> utf-8 worst case */
constant MDB-MAX-OBJ-NAME  = 256;
constant MDB-MAX-COLS      = 256;
constant MDB-MAX-IDX-COLS  = 10;
constant MDB-CATALOG-PG    = 18;
constant MDB-MEMO-OVERHEAD = 12;
constant MDB-BIND-SIZE     = 16384;

class MdbBackendType is repr('CStruct') {
    has str   $!name;         #    char *name;
    has uint8 $!needs-length; #    unsigned char needs_length; /* or precision */
    has uint8 $!needs-scale;  #    unsigned char needs_scale;
    has uint8 $!needs-quotes; #    unsigned char needs_quotes;
}

class MdbBackend is repr('CStruct') {
	has uint32 $!capabilities; # guint32 capabilities; # see MDB-SHEXP-* */
	has Pointer[MdbBackendType] $!types-table; # MdbBackendType *types-table;
	has Pointer[MdbBackendType] $!type-shortdate; # MdbBackendType *type-shortdate;
	has Pointer[MdbBackendType] $!type-autonum; # MdbBackendType *type-autonum;
	has str $!short-now; # const char *short-now;
	has str $!long-now; # const char *long-now;
	has Str $!charset-statement; # const char *charset-statement;
	has Str $!drop-statement; # const char *drop-statement;
	has Str $!constraint-not-empty-statement; # const char *constaint-not-empty-statement;
	has Str $!column-comment-statement; # const char *column-comment-statement;
	has Str $!table-comment-statement; # const char *table-comment-statement;
#	gchar* (*quote-schema-name)(const gchar*, const gchar*);
}

class MdbFileFlags is repr('CStruct') {
  has uint8 $!c;
}

class MdbFile is repr('CStruct') {
  has uint8 $!c;
}

# offset to row count on data pages...version dependant */
class MdbFormatConstants is repr('CStruct') {
	has uint32 $pg-size;                # ssize-t		pg-size;
	has uint16 $!row-count-offset;      # guint16		row-count-offset; 
	has uint16 $!tab-num-rows-offset;   # guint16		tab-num-rows-offset;
	has uint16 $!tab-num-cols-offset;   # guint16		tab-num-cols-offset;
	has uint16 $!tab-num-idxs-offset;   # guint16		tab-num-idxs-offset;
	has uint16 $!tab-num-ridxs-offset;  # guint16		tab-num-ridxs-offset;
	has uint16 $!tab-usage-map-offset;  # guint16		tab-usage-map-offset;
	has uint16 $!tab-first-dpg-offset;  # guint16		tab-first-dpg-offset;
	has uint16 $!tab-cols-start-offset; # guint16		tab-cols-start-offset;
	has uint16 $!tab-ridx-entry-size;   # guint16		tab-ridx-entry-size;
	has uint16 $!col-flags-offset;      # guint16		col-flags-offset;
	has uint16 $!col-size-offset;       # guint16		col-size-offset;
	has uint16 $!col-num-offset;        # guint16		col-num-offset;
	has uint16 $!tab-col-entry-size;    # guint16		tab-col-entry-size;
	has uint16 $!tab-free-map-offset;   # guint16         tab-free-map-offset;
	has uint16 $!tab-col-offset-var;    # guint16		tab-col-offset-var;
	has uint16 $!tab-col-offset-fixed;  # guint16		tab-col-offset-fixed;
	has uint16 $!tab-ro-col-num-offset; # guint16		tab-row-col-num-offset;
}

class MdbStatistics is repr('CStruct') {
	has uint8 $!collect;  # CHECK gboolean collect;
	has uint64 $pg-reads; #       unsigned long pg-reads;
}

class MdbHandle is repr('CStruct') {
    has Pointer[MdbFile]            $!f;               # MdbFile       *f;
    has uint32                      $!cur-pg;          # guint32       cur_pg;
    has uint16                      $!row-num;         # guint16       row_num;
    has uint32                      $!cur-pos;         # unsigned int  cur_pos;
    HAS int8 @!pg-buf[MDB-PGSIZE]     is CArray;       # unsigned char pg_buf[MDB_PGSIZE];
    HAS int8 @!alt-pg-buf[MDB-PGSIZE] is CArray;       # unsigned char alt_pg_buf[MDB_PGSIZE];
    has uint32                      $!num-catalog;     # unsigned int  num_catalog;
    has Pointer[GPtrArray]          $!catalog;         # GPtrArray       *catalog;
    has Pointer[MdbBackend]         $!default-backend; # MdbBackend      *default_backend;
    has Str                         $!backend-name;    # char            *backend_name;
    has Pointer[MdbFormatConstants] $!fmt;             # MdbFormatConstants *fmt;
    has Pointer[MdbStatistics]      $!stats;           # MdbStatistics *stats;
                                                       # iconv_t iconv_in;
                                                       # iconv_t iconv_out;
}


enum <
	MDB-PAGE-DB
	MDB-PAGE-DATA
	MDB-PAGE-TABLE
	MDB-PAGE-INDEX
	MDB-PAGE-LEAF
	MDB-PAGE-MAP
>;

enum (
	MDB-VER-JET3 => 0,
	MDB-VER-JET4 => 1,
	MDB-VER-ACCDB_2007 => 0x02,
	MDB-VER-ACCDB_2010 => 0x0103
);

enum <
	MDB-FORM
	MDB-TABLE
	MDB-MACRO
	MDB-SYSTEM-TABLE
	MDB-REPORT
	MDB-QUERY
	MDB-LINKED-TABLE
	MDB-MODULE
	MDB-RELATIONSHIP
	MDB-UNKNOWN-09
	MDB-UNKNOWN-0A # /* User access */
	MDB-DATABASE-PROPERTY
	MDB-ANY = -1
>;

enum (
	MDB-BOOL     => 0x01,
	MDB-BYTE     => 0x02,
	MDB-INT      => 0x03,
	MDB-LONGINT  => 0x04,
	MDB-MONEY    => 0x05,
	MDB-FLOAT    => 0x06,
	MDB-DOUBLE   => 0x07,
	MDB-DATETIME => 0x08,
	MDB-BINARY   => 0x09,
	MDB-TEXT     => 0x0a,
	MDB-OLE      => 0x0b,
	MDB-MEMO     => 0x0c,
	MDB-REPID    => 0x0f,
	MDB-NUMERIC  => 0x10,
	MDB-COMPLEX  => 0x12,
);

# SARG operators
enum <
	MDB-OR=1
	MDB-AND
	MDB-NOT
	MDB-EQUAL
	MDB-GT
	MDB-LT
	MDB-GTEQ
	MDB-LTEQ
	MDB-LIKE
	MDB-ISNULL
	MDB-NOTNULL
>;

enum MdbStrategy <
	MDB-TABLE-SCAN
	MDB-LEAF-SCAN
	MDB-INDEX-SCAN
>;

#enum MdbFileFlags <
#	MDB-NOFLAGS
#	MDB-WRITABLE
#>;

enum (
	MDB-DEBUG-LIKE  => 0x0001,
	MDB-DEBUG-WRITE => 0x0002,
	MDB-DEBUG-USAGE => 0x0004,
	MDB-DEBUG-OLE   => 0x0008,
	MDB-DEBUG-ROW   => 0x0010,
	MDB-DEBUG-PROPS => 0x0020,
	MDB-USE-INDEX   => 0x0040,
	MDB-NO-MEMO     => 0x0080, # don't follow memo fields
);

# #define mdb-is-logical-op(x) (x == MDB-OR || #
# 				x == MDB-AND || #
# 				x == MDB-NOT )
# 
# #define mdb-is-relational-op(x) (x == MDB-EQUAL || #
# 				x == MDB-GT || #
# 				x == MDB-LT || #
# 				x == MDB-GTEQ || #
# 				x == MDB-LTEQ || #
# 				x == MDB-LIKE || #
# 				x == MDB-ISNULL || #
# 				x == MDB-NOTNULL )
 
enum <
	MDB-ASC
	MDB-DESC
>;

enum (
	MDB-IDX-UNIQUE      => 0x01,
	MDB-IDX-IGNORENULLS => 0x02,
	MDB-IDX-REQUIRED    => 0x08 
);

# export schema options
enum (
	MDB-SHEXP-DROPTABLE    => 1 +< 0, # issue drop table during export */
	MDB-SHEXP-CST-NOTNULL  => 1 +< 1, # generate NOT NULL constraints */
	MDB-SHEXP-CST-NOTEMPTY => 1 +< 2, # <>'' constraints */
	MDB-SHEXP-COMMENTS     => 1 +< 3, # export comments on columns & tables */
	MDB-SHEXP-DEFVALUES    => 1 +< 4, # export default values */
	MDB-SHEXP-INDEXES      => 1 +< 5, # export indices */
	MDB-SHEXP-RELATIONS    => 1 +< 6, # export relation (foreign keys) */
);
#define MDB-SHEXP-DEFAULT (MDB-SHEXP-CST-NOTNULL | MDB-SHEXP-COMMENTS | MDB-SHEXP-INDEXES | MDB-SHEXP-RELATIONS)

# csv export binary options
enum <
	MDB-BINEXPORT-STRIP
	MDB-BINEXPORT-RAW
	MDB-BINEXPORT-OCTAL
>;

#define IS-JET4(mdb) (mdb->f->jet-version==MDB-VER-JET4) # obsolete
#define IS-JET3(mdb) (mdb->f->jet-version==MDB-VER-JET3)

# forward declarations */
#typedef struct mdbindex MdbIndex;
#typedef struct mdbsargtree MdbSargNode;

class GHashTable is repr('CStruct') {
  has int32         $!size;                  # gsize            size;
  has int32         $!mod;                   # gint             mod;
  has uint32        $!mask;                  # guint            mask;
  has  int32        $!nnodes;                # gint             nnodes;
  has  int32        $!noccupied;             # gint             noccupied;  /* nnodes + tombstones */

  has uint32        $!have-big-keys;         # guint            have_big_keys : 1;
  has uint8         $!have-big-values;       # guint            have_big_values : 1; ### FIX

  has Pointer       $!keys;                  # gpointer         keys;
  has uint32        $!hashes;                # guint           *hashes;
  has Pointer       $!values;                # gpointer         values;

  has GHashFunc     &!hash_func;             # GHashFunc        hash_func;
  has GEqualFunc    &!key-equal-func;        # GEqualFunc       key_equal_func;
  has uint32        $!ref-count;             # gatomicrefcount  ref_count;

# delete if not compiled in to library:
#ifndef G_DISABLE_ASSERT
  /*
   * Tracks the structure of the hash table, not its contents: is only
   * incremented when a node is added or removed (is not incremented
   * when the key or data of a node is modified).
   */
  has int32          $!version;              # int              version;
#endif
  has GDestroyFunc   &!key-destroyfunc;      # GDestroyNotify   key_destroy_func;
  has GDestroyNotify &!value-destroy-Notify; # GDestroyNotify   value_destroy_func;
}

class GArray is repr('CStruct') { ### FIX
  has Pointer[int8] $data;
  has uint32        $len;
}

class MdbProperties is repr('CStruct') {
	has Str                 $!name; # gchar *name;
	has Pointer[GHashTable] $!hash; # GHashTable	*hash;
}

class MdbCatalogEntry is repr('CStruct') {
	has Pointer[MdbHandle]     $!mdb;                       # MdbHandle	*mdb;
	HAS int8 @!object-name[MDB-MAX-OBJ-NAME - 1] is CArray; # char object-name[MDB-MAX-OBJ-NAME+1];
	has uint32                 $!object-type;               # int            object-type;
	has uint64                 $!table-pg;                  # unsigned long  table-pg; # misnomer since object may not be a table
	                                                        # //int          num-props; please use props->len
	has Pointer[GArray]        $!props;                     # GArray		*props; # GArray of MdbProperties
	has Pointer[GArray]        $!columns;                   # GArray		*columns;
	has int32                  $!flags;                     # int		flags;
}

class MdbAny is repr('CUnion') {
	has int32 $!i;               # int	i;
	has num64 $!d;               # double	d;
	HAS int8 @!s[256] is CArray; # char	s[256];
}

#struct S-MdbTableDef; # forward definition */
class MdbColumn is repr('CStruct') {
	HAS int8 @!name[MDB-MAX-OBJ-NAME+1] is CArray; # char		name[MDB-MAX-OBJ-NAME+1];
	has int32                 $!col-type;          # int		col-type;
	has int32                 $!col-size;          # int		col-size;
	has Pointer               $!bind-ptr;          # void	*bind-ptr;
	has Pointer[int32]        $!len-ptr;           # int		*len-ptr;
	has Pointer[GHashTable]   $!properties;        # GHashTable	*properties;
	has uint32                $!num-sargs;         # unsigned int	num-sargs;
	has Pointer[GPtrArray]    $!sargs;             # GPtrArray	*sargs;
	has Pointer[GPtrArray]    $!idx-sarg-cache;    # GPtrArray	*idx-sarg-cache;
	has uint8                 $!is-fixed;          # unsigned char   is-fixed;
	has int32                 $!query-order;       # int		query-order;
             # col-num is the current column order, 
             # does not include deletes
	has int32                 $!col-num;           # int		col-num;	
	has int32                 $!cur-value-start;   # int		cur-value-start;
	has int32                 $!cur-value-len;     # int 		cur-value-len;
	# MEMO/OLE readers
	has uint32                $!cur-blob-pg-row;   # guint32		cur-blob-pg-row;
	has int32                 $!chunk-size;        # int		chunk-size;
	# numerics only
	has int32                 $!col-prec;          # int		col-prec;
	has int32                 $!col-scale;         # int		col-scale;
	has uint8                 $!is-long-auto;      # unsigned char     is-long-auto;
	has uint8                 $!is-uuid-auto;      # unsigned char     is-uuid-auto;
	has Pointer[MdbProperties] $!props;            # MdbProperties	*props;
	# info needed for handling deleted/added columns */
	has int32                 $!fixed-offset;      # int 		fixed-offset;
	has uint32                $!var-col-num;       # unsigned int	var-col-num;
	# row-col-num is the row column number order, including deleted columns */
	has int32                 $!row-col-num;       # int		row-col-num;
}

class MdbSargNode is repr('CStruct') {
	has int32                $!op;     # int       op;
	has Pointer[MdbColumn]   $!col;    # MdbColumn *col;
	has MdbAny               $!value;  # MdbAny    value;
	has Pointer              $!parent; # void      *parent;
#	has Pointer[MdbSargNode] $!left;   # MdbSargNode *left;  ### FIX
#	has Pointer[MdbSargNode] $!right;  # MdbSargNode *right; ### FIX
}

class MdbIndexPage is repr('CStruct') {
	has uint32 $!pg;                         # guint32 pg;
	has int32  $!start-pos;                  # int start-pos;
	has int32  $!offset;                     # int offset;
	has int32  $!len;                        # int len;
	HAS int16  @!idx-starts[2000] is CArray; # guint16 idx-starts[2000];	
	HAS uint8  @!cache-value[256] is CArray; # unsigned char cache-value[256];
}

### FIX typedef int (*MdbSargTreeFunc)(MdbSargNode *, gpointer data);

constant MDB-MAX-INDEX-DEPTH = 10;

class MdbIndexChain is repr('CStruct') {
	has  int32       $!cur-depth;                            # int cur-depth;
	has uint32       $!last-leaf-found;                      # guint32 last-leaf-found;
	has  int32       $!clean-up-mode;                        # int clean-up-mode;
	HAS MdbIndexPage @!pages[MDB-MAX-INDEX-DEPTH] is CArray; # MdbIndexPage pages[MDB-MAX-INDEX-DEPTH];
}

# typedef struct S-MdbTableDef {
class MdbTableDef is repr('CStruct') {
	has Pointer[MdbCatalogEntry] $!entry;                              # MdbCatalogEntry *entry;
	has  int8                    @!name[MDB-MAX-OBJ-NAME+1] is CArray; # char	name[MDB-MAX-OBJ-NAME+1];
	has  int32                   $!num-cols;                           # unsigned int    num-cols;
#	has Pointer[GPtrArray]       $!columns;                            # GPtrArray	*columns;
	has uint32                   $!num-rows;                           # unsigned int    num-rows;
	has  int32                   $!index-start;                        # int	index-start;
	has uint32                   $!num-real-idxs;                      # unsigned int    num-real-idxs;
	has uint32                   $!num-idxs;                           # unsigned int    num-idxs;
#	has Pointer[GPtrArray]       $!indices;                            # GPtrArray	*indices;
	has uint32                   $!first-data-pg;                      # guint32	first-data-pg;
	has uint32                   $!cur-pg-num;                         # guint32	cur-pg-num;
	has uint32                   $!cur-phys-pg;                        # guint32	cur-phys-pg;
	has uint32                   $!cur-row;                            # unsigned int    cur-row;
	has  int32                   $!noskip-del;                         # int  noskip-del;  # don't skip deleted rows
	# object allocation map
	has uint32                   $!map-base-pg;                        # guint32  map-base-pg;
	has uint32                   $!map-sz;                             # size-t map-sz;
	has Pointer[uint8]           $!usage-map;                          #         unsigned char *usage-map;
	# pages with free space left
	has uint32                   $!freemap-base-pg;                    # guint32  freemap-base-pg;
	has uint32                   $!freemap-sz;                         # size-t freemap-sz;
	has Pointer[uint8]           $!free-usage-map;                     # unsigned char *free-usage-map;
	# query planner
	has Pointer[MdbSargNode]     $!sarg-tree;                          # MdbSargNode *sarg-tree;
	has int32                    $!strategy;                           # MdbStrategy strategy; ### FIX
#	has Pointer[MdbIndex]        $!scan-idx;                           # MdbIndex *scan-idx; ### FIX
	has Pointer[MdbHandle]       $!mdbidx;                             # MdbHandle *mdbidx;
	has Pointer[MdbIndexChain]   $!chain;                              # MdbIndexChain *chain;
	has Pointer[MdbProperties]   $!props;                              # MdbProperties	*props;
	has uint32                   $!num-var-cols;                       # unsigned int num-var-cols;  # to know if row has variable columns
	# temp table
	has uint32                   $!is-temp-table;                      # unsigned int  is-temp-table;
	has Pointer[GPtrArray]       $!temp-table-pages;                   # GPtrArray     *temp-table-pages;
}

#struct mdbindex {
class MdbIndex is repr('CStruct') {
	has  int32               $!index-num;                                   # int		index-num;
	has  int8                @!name[MDB-MAX-OBJ-NAME+1]          is CArray; # char		name[MDB-MAX-OBJ-NAME+1];
	has uint8                $!index-type;                                  # unsigned char	index-type;
	has uint32               $!first-pg;                                    # guint32		first-pg;
	has  int32               $!num-rows;                                    # int		num-rows;  # number rows in index
	has uint32               $!num-keys;                                    # unsigned int	num-keys;
	has  int16               @!key-col-num[MDB-MAX-IDX-COLS]     is CArray; # short	key-col-num[MDB-MAX-IDX-COLS];
	has uint8                @!key-col-order[MDB-MAX-IDX-COLS]   is CArray; # unsigned char	key-col-order[MDB-MAX-IDX-COLS];
	has uint8                $!flags;                                       # unsigned char	flags;
	has Pointer[MdbTableDef] $!table;                                       # MdbTableDef	*table; ### FIX
}

class MdbColumnProp is repr('CStruct') {
	has int8 @!name[MDB-MAX-OBJ-NAME+1] is CArray; # char		name[MDB-MAX-OBJ-NAME+1];
}

class MdbField is repr('CStruct') {
	has Pointer $!value;    # void *value;
	has  int32  $!siz;      # int siz;
	has  int32  $!start;    # int start;
	has uint8   $!is-null;  # unsigned char is-null;
	has uint8   $!is-fixed; # unsigned char is-fixed;
	has  int32  $!column;   # int colnum;
	has  int32  $!offset;   # int offset;
}

class MdbSarg is repr('CStruct') {
	has int32  $!op;    # int	op;
	has MdbAny $!value; # MdbAny	value;
}

# mem.c
# extern MDB-DEPRECATED(void, mdb-init());
# extern MDB-DEPRECATED(void, mdb-exit());

# file.c
    # extern ssize-t mdb-read-pg(MdbHandle *mdb, unsigned long pg);
sub mdb-read-pg(Pointer[MdbHandle] $mdb,
                uint64             $pg)
          returns uint32
          is native('mdb')
          is symbol('mdb_read_pg')
        { * }

    # extern ssize-t mdb-read-alt-pg(MdbHandle *mdb, unsigned long pg);
sub mdb-read-alt-pg(Pointer[MdbHandle] $mdb,
                    uint64             $pg)
          returns uint32
          is native('mdb')
          is symbol('mdb_read_alt_pg')
        { * }

    # extern unsigned char mdb-get-byte(void *buf, int offset);
sub mdb-get-byte(Pointer $buf,
                 int32   $offset)
          returns uint8
          is native('mdb')
          is symbol('mdb_get_byte')
        { * }

    # extern int    mdb-get-int16(void *buf, int offset);
sub mdb-get-int16(Pointer $buf,
                  int32   $offset)
          returns int32
          is native('mdb')
          is symbol('mdb_get_int16')
        { * }

    # extern long   mdb-get-int32(void *buf, int offset);
sub mdb-get-int32(Pointer $buf,
                  int32   $offset)
          returns int64
          is native('mdb')
          is symbol('mdb_get_int32')
        { * }

    # extern long   mdb-get-int32-msb(void *buf, int offset);
sub mdb-get-int32-msb(Pointer $buf,
                      int32   $offset)
          returns int64
          is native('mdb')
          is symbol('mdb_get_int32_msb')
        { * }

    # extern float  mdb-get-single(void *buf, int offset);
sub mdb-get-single(Pointer $buf,
                   int32   $offset)
          returns num32
          is native('mdb')
          is symbol('mdb_get_single')
        { * }

    # extern double mdb-get-double(void *buf, int offset);
sub mdb-get-double(Pointer $buf,
                   int32   $offset)
          returns num64
          is native('mdb')
          is symbol('mdb_get_double')
        { * }

    # extern unsigned char mdb-pg-get-byte(MdbHandle *mdb, int offset);
sub mdb-pg-get-byte(Pointer[MdbHandle] $mdb,
                    int32              $offset)
          returns uint8
          is native('mdb')
          is symbol('mdb_pg_get_byte')
        { * }

    # extern int    mdb-pg-get-int16(MdbHandle *mdb, int offset);
sub mdb-pg-get-int16(Pointer[MdbHandle] $mdb,
                     int32              $offset)
          returns int32
          is native('mdb')
          is symbol('mdb_pg_get_int16')
        { * }

    # extern long   mdb-pg-get-int32(MdbHandle *mdb, int offset);
sub mdb-pg-get-int32(Pointer[MdbHandle] $mdb,
                     int32              $offset)
          returns int64
          is native('mdb')
          is symbol('mdb_pg_get_int32')
        { * }

    # extern float  mdb-pg-get-single(MdbHandle *mdb, int offset);
sub mdb-pg-get-single(Pointer[MdbHandle] $mdb,
                      int32              $offset)
          returns num32
          is native('mdb')
          is symbol('mdb_pg_get_single')
        { * }

    # extern double mdb-pg-get-double(MdbHandle *mdb, int offset);
sub mdb-pg-get-double(Pointer[MdbHandle] $mdb,
                      int32              $offset)
          returns num64
          is native('mdb')
          is symbol('mdb_pg_get_double')
        { * }


    # extern MdbHandle *mdb_open(const char *filename, MdbFileFlags flags);
sub mdb-open(Str          $filename,
             MdbFileFlags $flags)
          returns Pointer[MdbHandle]
          is native('mdb')
          is symbol('mdb_open')
        { * }

    # extern void mdb-close(MdbHandle *mdb);
sub mdb-close(Pointer[MdbHandle] $mdb)
          is native('mdb')
          is symbol('db_close')
        { * }

    # extern MdbHandle *mdb-clone-handle(MdbHandle *mdb);
sub mdb-clone-handle(Pointer[MdbHandle] $mdb)
          returns Pointer[MdbHandle]
          is native('mdb')
          is symbol('mdb_clone_handle')
        { * }

    # extern void mdb-swap-pgbuf(MdbHandle *mdb);
sub mdb-swap-pgbuf(Pointer[MdbHandle] $mdb)
          is native('mdb')
          is symbol('mdb_swap_pgbuf')
        { * }

# catalog.c */
    # extern void mdb-free-catalog(MdbHandle *mdb);
    # extern GPtrArray *mdb-read-catalog(MdbHandle *mdb, int obj-type);
    # MdbCatalogEntry *mdb-get-catalogentry-by-name(MdbHandle *mdb, const gchar* name);
    # extern void mdb-dump-catalog(MdbHandle *mdb, int obj-type);
    # extern char *mdb-get-objtype-string(int obj-type);

# table.c */
    # extern MdbTableDef *mdb-alloc-tabledef(MdbCatalogEntry *entry);
    # extern void mdb-free-tabledef(MdbTableDef *table);
    # extern MdbTableDef *mdb-read-table(MdbCatalogEntry *entry);
    # extern MdbTableDef *mdb-read-table-by-name(MdbHandle *mdb, gchar *table-name, int obj-type);
    # extern void mdb-append-column(GPtrArray *columns, MdbColumn *in-col);
    # extern void mdb-free-columns(GPtrArray *columns);
    # extern GPtrArray *mdb-read-columns(MdbTableDef *table);
    # extern void mdb-table-dump(MdbCatalogEntry *entry);
    # extern guint8 read-pg-if-8(MdbHandle *mdb, int *cur-pos);
    # extern guint16 read-pg-if-16(MdbHandle *mdb, int *cur-pos);
    # extern guint32 read-pg-if-32(MdbHandle *mdb, int *cur-pos);
    # extern void *read-pg-if-n(MdbHandle *mdb, void *buf, int *cur-pos, size-t len);
    # extern int mdb-is-user-table(MdbCatalogEntry *entry);
    # extern int mdb-is-system-table(MdbCatalogEntry *entry);
    # extern const char *mdb-table-get-prop(const MdbTableDef *table, const gchar *key);
    # extern const char *mdb-col-get-prop(const MdbColumn *col, const gchar *key);

# data.c */
    # extern int mdb-bind-column-by-name(MdbTableDef *table, gchar *col-name, void *bind-ptr, int *len-ptr);
    # extern void mdb-data-dump(MdbTableDef *table);
    # extern void mdb-date-to-tm(double td, struct tm *t);
    # extern void mdb-bind-column(MdbTableDef *table, int col-num, void *bind-ptr, int *len-ptr);
    # extern int mdb-rewind-table(MdbTableDef *table);
    # extern int mdb-fetch-row(MdbTableDef *table);
    # extern int mdb-is-fixed-col(MdbColumn *col);
    # extern char *mdb-col-to-string(MdbHandle *mdb, void *buf, int start, int datatype, int size);
    # extern int mdb-find-pg-row(MdbHandle *mdb, int pg-row, void **buf, int *off, size-t *len);
    # extern int mdb-find-row(MdbHandle *mdb, int row, int *start, size-t *len);
    # extern int mdb-find-end-of-row(MdbHandle *mdb, int row);
    # extern int mdb-col-fixed-size(MdbColumn *col);
    # extern int mdb-col-disp-size(MdbColumn *col);
    # extern size-t mdb-ole-read-next(MdbHandle *mdb, MdbColumn *col, void *ole-ptr);
    # extern size-t mdb-ole-read(MdbHandle *mdb, MdbColumn *col, void *ole-ptr, int chunk-size);
    # extern void* mdb-ole-read-full(MdbHandle *mdb, MdbColumn *col, size-t *size);
    # extern void mdb-set-date-fmt(const char *);
    # extern void mdb-set-boolean-fmt-words();
    # extern int mdb-read-row(MdbTableDef *table, unsigned int row);

# dump.c */
    # extern void mdb-buffer-dump(const void *buf, int start, size-t len);

# backend.c */
    # extern MDB-DEPRECATED(char*, mdb-get-coltype-string(MdbBackend *backend, int col-type));
    # extern MDB-DEPRECATED(int, mdb-coltype-takes-length(MdbBackend *backend, int col-type));
    # extern const MdbBackendType* mdb-get-colbacktype(const MdbColumn *col);
    # extern const char* mdb-get-colbacktype-string(const MdbColumn *col);
    # extern int mdb-colbacktype-takes-length(const MdbColumn *col);
    # extern MDB-DEPRECATED(void, mdb-init-backends());
    # extern void mdb-register-backend(char *backend-name, guint32 capabilities, MdbBackendType *backend-type, MdbBackendType *type-shortdate, MdbBackendType *type-autonum, const char *short-now, const char *long-now, const char *charset-statement, const char *drop-statement, const char *constaint-not-empty-statement, const char *column-comment-statement, const char *table-comment-statement, gchar* (*quote-schema-name)(const gchar*, const gchar*));
    # extern MDB-DEPRECATED(void, mdb-remove-backends());
    # extern int  mdb-set-default-backend(MdbHandle *mdb, const char *backend-name);
    # extern void mdb-print-schema(MdbHandle *mdb, FILE *outfile, char *tabname, char *dbnamespace, guint32 export-options);

# sargs.c */
    # extern int mdb-test-sargs(MdbTableDef *table, MdbField *fields, int num-fields);
    # extern int mdb-test-sarg(MdbHandle *mdb, MdbColumn *col, MdbSargNode *node, MdbField *field);
    # extern void mdb-sql-walk-tree(MdbSargNode *node, MdbSargTreeFunc func, gpointer data);
    # extern int mdb-find-indexable-sargs(MdbSargNode *node, gpointer data);
    # extern int mdb-add-sarg-by-name(MdbTableDef *table, char *colname, MdbSarg *in-sarg);
    # extern int mdb-test-string(MdbSargNode *node, char *s);
    # extern int mdb-test-int(MdbSargNode *node, gint32 i);
    # extern int mdb-add-sarg(MdbColumn *col, MdbSarg *in-sarg);



# index.c */
    # extern GPtrArray *mdb-read-indices(MdbTableDef *table);
    # extern void mdb-index-dump(MdbTableDef *table, MdbIndex *idx);
    # extern void mdb-index-scan-free(MdbTableDef *table);
    # extern int mdb-index-find-next-on-page(MdbHandle *mdb, MdbIndexPage *ipg);
    # extern int mdb-index-find-next(MdbHandle *mdb, MdbIndex *idx, MdbIndexChain *chain, guint32 *pg, guint16 *row);
    # extern void mdb-index-hash-text(char *text, char *hash);
    # extern void mdb-index-scan-init(MdbHandle *mdb, MdbTableDef *table);
    # extern int mdb-index-find-row(MdbHandle *mdb, MdbIndex *idx, MdbIndexChain *chain, guint32 pg, guint16 row);
    # extern void mdb-index-swap-n(unsigned char *src, int sz, unsigned char *dest);
    # extern void mdb-free-indices(GPtrArray *indices);
    # void mdb-index-page-reset(MdbIndexPage *ipg);
    # extern int mdb-index-pack-bitmap(MdbHandle *mdb, MdbIndexPage *ipg);

# stats.c */
    # extern void mdb-stats-on(MdbHandle *mdb);
    # extern void mdb-stats-off(MdbHandle *mdb);
    # extern void mdb-dump-stats(MdbHandle *mdb);

# like.c */
    # extern int mdb-like-cmp(char *s, char *r);

# write.c */
    # extern void mdb-put-int16(void *buf, guint32 offset, guint32 value);
    # extern void mdb-put-int32(void *buf, guint32 offset, guint32 value);
    # extern void mdb-put-int32-msb(void *buf, guint32 offset, guint32 value);
    # extern int mdb-crack-row(MdbTableDef *table, int row-start, int row-end, MdbField *fields);
    # extern guint16 mdb-add-row-to-pg(MdbTableDef *table, unsigned char *row-buffer, int new-row-size);
    # extern int mdb-update-index(MdbTableDef *table, MdbIndex *idx, unsigned int num-fields, MdbField *fields, guint32 pgnum, guint16 rownum);
    # extern int mdb-insert-row(MdbTableDef *table, int num-fields, MdbField *fields);
    # extern int mdb-pack-row(MdbTableDef *table, unsigned char *row-buffer, unsigned int num-fields, MdbField *fields);
    # extern int mdb-replace-row(MdbTableDef *table, int row, void *new-row, int new-row-size);
    # extern int mdb-pg-get-freespace(MdbHandle *mdb);
    # extern int mdb-update-row(MdbTableDef *table);
    # extern void *mdb-new-data-pg(MdbCatalogEntry *entry);

# map.c */
    # extern guint32 mdb-map-find-next-freepage(MdbTableDef *table, int row-size);
    # extern gint32 mdb-map-find-next(MdbHandle *mdb, unsigned char *map, unsigned int map-sz, guint32 start-pg);

# props.c */
    # extern void mdb-free-props(MdbProperties *props);
    # extern void mdb-dump-props(MdbProperties *props, FILE *outfile, int show-name);
    # extern GArray* mdb-kkd-to-props(MdbHandle *mdb, void *kkd, size-t len);


# worktable.c */
    # extern MdbTableDef *mdb-create-temp-table(MdbHandle *mdb, char *name);
    # extern void mdb-temp-table-add-col(MdbTableDef *table, MdbColumn *col);
    # extern void mdb-fill-temp-col(MdbColumn *tcol, char *col-name, int col-size, int col-type, int is-fixed);
    # extern void mdb-fill-temp-field(MdbField *field, void *value, int siz, int is-fixed, int is-null, int start, int column);
    # extern void mdb-temp-columns-end(MdbTableDef *table);

# options.c */
    # extern int mdb-get-option(unsigned long optnum);
    # extern void mdb-debug(int klass, char *fmt, ...);

# iconv.c */
    # extern int mdb-unicode2ascii(MdbHandle *mdb, char *src, size-t slen, char *dest, size-t dlen);
    # extern int mdb-ascii2unicode(MdbHandle *mdb, char *src, size-t slen, char *dest, size-t dlen);
    # extern void mdb-iconv-init(MdbHandle *mdb);
    # extern void mdb-iconv-close(MdbHandle *mdb);
    # extern const char* mdb-target-charset(MdbHandle *mdb);
