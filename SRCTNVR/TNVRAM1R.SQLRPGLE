000100000414     H DECEDIT('0,') DATEDIT(*DMY.)
000200000414
000300160211     FWFAZD00F  O    E             DISK
000400160211
000500160211     D WFAMAZINDF    e ds
000600001120     D*-----------------
000700001120     D* VARIABILI DI WRK
000800001120     D*-----------------
000900160211     d wSql            s           4096    inz  varying
001000160211     d data            s               d
001100160211     d ora             s               t
001200140722
001300140722     D*------------------
001400140722     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
001500140722     D*------------------
001600140722     D InStringa       S          65535A   VARYING                              (stringa input)
001700140722     D InSepara        S             10A                                        (separatore)
001800140722     D InTypeOut       S              1A                                        (tipo output)
001900140722     D wSkParam        S          65535A                                        (output)
002000140722     D OutErrore       S              1A                                        (flag errore)
002100140722     D OutMsg          S             80A                                        (messaggio errore)
002200140722     D SkSplitCSV_4    S            128    DIM(512)
002300140722
002400000414     C*------------------------------------------------------------------------*
002500000414     C* MAIN LINE
002600000414     C*------------------------------------------------------------------------*
002700160211
002800160211      /free
002900160211       data = %date();
003000160211       ora = %time();
003100160211
003200160211       wSQL =
003300160211             'select * from  wfamazindf';
003400160211       exec sql  prepare S1  from :wSql;
003500160211       exec sql  declare C1  cursor for S1;
003600160211       exec sql  open C1;
003700160211
003800160211       DoU  SQLCode = 100 or SQLCode < 0;
003900160211         exec sql  fetch next  from C1  into :WFAMAZINDF;
004000160211
004100160211         select;
004200160211         // errore
004300160211         when SQLCode < 0;
004400160211           dsply 'ERRORE SQL';
004500160211           // - Chiusura cursore (visto che esco dal pgm)
004600160211           exec sql   close C1;
004700160211           *inlr = *on;
004800160211           return;
004900160211         // nessuna lettura
005000160211         when SQLCode = 100;
005100160211           // esco dal ciclo
005200160211         // lettura
005300160211         when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
005400160211           // splitto il campo INDIRIZZO e scrivo il file WFAZD00F
005500160211           exsr esegui;
005600160211         // fine lettura
005700160211         other;
005800160211           // non faccio nulla
005900160211         endsl;
006000160211
006100160211       enddo;
006200160211
006300160211       *inlr = *on;
006400160211      /end-free
006500160211     C*------------------------------------------------------------------------*
006600160211     C* ESEGUI - esecuzione operazioni
006700160211     C*------------------------------------------------------------------------*
006800160211     C     ESEGUI        BEGSR
006900160211     C*
007000160211     C* sostituisco eventuali duple di controllo (?'  ?+  ??  ?.) con blank
007100160211     C                   eval      INDIRIZZO = %scanrpl('?''':' ':INDIRIZZO)
007200160211     C                   eval      INDIRIZZO = %scanrpl('?+' :' ':INDIRIZZO)
007300160211     C                   eval      INDIRIZZO = %scanrpl('?:' :' ':INDIRIZZO)
007400160211     C                   eval      INDIRIZZO = %scanrpl('??' :' ':INDIRIZZO)
007500160211     C                   eval      INDIRIZZO = %scanrpl('?.' :' ':INDIRIZZO)
007600140722     C*
007700160211     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
007800160211     C                   eval      InStringa = %trim(Indirizzo)
007900140722     C*
008000140722     C                   CALL      'XSPLIT2'
008100140722     C                   PARM                    InStringa
008200160211     C                   PARM      '+'           InSepara
008300140722     C                   PARM      '4'           InTypeOut
008400140722     C                   PARM      ''            wSkParam
008500140722     C                   PARM                    OutErrore
008600140722     C                   PARM                    OutMsg
008700140722     C                   MOVEA     wSkParam      SkSplitCSV_4
008800140722     C*
008900140722     C* Valorizzo la schiera delle intestazioni colonne (elementi)
009000160211     C                   if        wSkParam <> *blanks
009100160211     C                   clear                   WFAZD000
009200160211     C                   eval      Dest_Ind = %trim(SkSplitCSV_4(6))
009300160211     C                   eval      Dest_Lod = %trim(SkSplitCSV_4(7))
009400160211     C                   eval      Dest_Prd = %trim(SkSplitCSV_4(8))
009500160211     C                   eval      Dest_Cad = %trim(SkSplitCSV_4(9))
009600160211     C                   write     WFAZD000
009700041215     C                   ENDIF
009800001120     C*
009900000414     C                   ENDSR
