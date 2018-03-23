000100940224     H*--------------------------------------------------------------*
000200130114      *   Allinea il campo della Società x la Ristampa sul FILLER1
000300130114      *   ci sono dei ragionamenti da fare sulle società cambiate
000400130114      *   o trascodificate.
000500130114     H*--------------------------------------------------------------*
000600130114     FAITRS00F  UF   E             DISK
000700900131     C*---------------------------------------------------------------*
000800130114     D dtrsflr1      E DS
000900130114     D exsocieta       s              3
001000130114     d VALUE_IND       s              5i 0
001100130114     d fine_validita...
001200130114     d                 s               d   inz(d'2012-12-30') datfmt(*iso)
001300130114     d inizio_validita...
001400130114     d                 s               d   inz(d'2012-12-31') datfmt(*iso)
001500130114     d dataiso_Stampa  s               d   datfmt(*iso)
001600130114     d dataiso         s               d   datfmt(*iso)
001700030205     C*---------------------------------------------------------------*
001800030515     c                   do        *hival
001900130114     c                   read      aitrs00f
002000030515     c* fine file
002100130114     c                   if        %eoF(aitrs00f)
002200030515     c                   leave
002300030515     c                   end
002400130114     C*
002500130114     C*  carica DS filler
002600130114     c                   eval      dTRSflr1  =   trsFLR1
002700130114     C*
002800130114     C* SOLO I CAMPI CON SOCIETA BLANKS
002900130114     C*  CampoSocietà x ristampa se già modificato leggo il prossimo
003000130114     c                   if        §TRSSOCPRT <> *blank
003100130114     c                   iter
003200130114     c                   end
003300130114     C*
003400130114     C*  imposto la stessa società del record come DEFAULT
003500130114     c                   eval      §TRSSOCPRT = trsSOCG
003600130114     C*
003700130114      **  eccezioni per impostare la società
003800130114      **                        **--------------**
003900130114     c                   exsr      CASI_SOCieta
004000130114      **                        **--------------**
004100130114      **
004200130115      **  Se NON è stato stampato non deve riportare nulla
004300130115     c                   if        trsDSC = 0
004400130115     c                   eval          §TRSSOCPRT = *blank
004500130115     c                   end
004600130115      **
004700130114      **   reimposto la DS  da Aggiornare
004800130114     c                   eval      trsFLR1 =    dTRSflr1
004900130114     C*
005000130114     c                   update    aitrs000
005100030205     c                   enddo
005200000616     C*
005300000000     C                   SETON                                        LR
005400130114     C*---------------------------------------------------------------------------------
005500130114     C*   particolarità di trascodifica
005600130114     C*---------------------------------------------------------------------------------
005700130114     c     CASI_SOCieta  begsr
005800130114     C*
005900130114     c                   eval      exSocieta = trsSOCG
006000130114     C*
006100130114     C* Solo DATA STAMPA piena
006200130114     c                   if        trsDSC > 0
006300130114     C*
006400130114     c     *iso          move      trsDSC        dataiso_Stampa
006500130114     C*
006600130114     c* se la data di stampa del contratto è < del 31/12/2012
006700130114     C*   devo intervenire con dei ragionamenti in base alla società presente sul
006800130114     C* record ed al file che ha traccia della trascodifica.
006900130114      **               --------------------------------
007000130114     c                   if        dataiso_Stampa < inizio_validita
007100130114      **               --------------------------------
007200130114      **
007300130114      **  Controlla presenza della Società TRASCODIFICATA sul file di WORK
007400130114      **    e solo se presente PROSEGUE sull'AZSOF x reperire la SOC Vecchia
007500130114     c                   z-add     0             TotRecords        9 0
007600130114     C/EXEC SQL
007700130114     C+ SELECT count(*) into :TotRecords
007800130114     C+    FROM YUSOC00F WHERE
007900130114     C+                        SOCSOCNEW = :trsSOCG
008000130114     C+                  AND   SOCDTCHIU = :fine_validita
008100130114     C+                  AND   SOCDTAPER = :inizio_validita
008200130114     C/END-EXEC
008300130114     c                   if        SQLCOD =0 and
008400130114     c                             TotRecords > 0
008500130114      *
008600130114      ** cerca la vecchia Società
008700130114     c                   exsr      CERCA_VECCHIA
008800130114      *
008900130114      **
009000130114     c                   end
009100130114      **               --------------------------------
009200130114     c                   end
009300130114      **               --------------------------------
009400130114     c                   endIF
009500130114     C*
009600130114     c                   endsr
009700130114     C*---------------------------------------------------------------------------------
009800130114     C*  x altro tipo di società occorre fare riferimento al file di trasferimento
009900130114     C*---------------------------------------------------------------------------------
010000130114     c     CERCA_VECCHIA Begsr
010100130114     C*
010200130114     c                   move      '0'           TROVATO_SQL       1
010300130114      **
010400130114      **  Verifica esistenza su AZSOF con la TRASCODIFICATA
010500130114      **    poi prosegue x arrivare alla Vecchia Società tramite il record scaduto
010600130114     C/EXEC SQL
010700130114     C+ SELECT '1' into :TROVATO_SQL INDICATOR :VALUE_IND
010800130114     C+    FROM AZSOF00F WHERE
010900130114     C+                        ID_SOCIETA = :trsSOCG
011000130114     C+                  AND   ID_FILIALE = :trsFIL
011100130114     C+                  AND   VALIDITA_DATA_INIZIO = :inizio_validita
011200130114     C/END-EXEC
011300130114     c                   if        SQLCOD =0 and VALUE_IND =0 and
011400130114     c                             TROVATO_SQL ='1'
011500130114      **
011600130114      ** Adesso prova con la data scadenza
011700130114     c                   move      '0'           TROVATO_SQL       1
011800130114     C/EXEC SQL
011900130114     C+ SELECT ID_SOCIETA, '1' into :exSocieta, :TROVATO_SQL
012000130114     C+                             INDICATOR :VALUE_IND
012100130114     C+    FROM AZSOF00F WHERE
012200130114     C+                        ID_FILIALE = :trsFIL
012300130114     C+                  AND   VALIDITA_DATA_FINE = :fine_validita
012400130114     C/END-EXEC
012500130114      **      ha trovato ed impostata la società
012600130114     c                   if        SQLCOD =0 and VALUE_IND =0 and
012700130114     c                             TROVATO_SQL ='1'
012800130114      **
012900130114     C* imposta il campo della DS Filler1 solo se entrato in < 2012-12-31
013000130114     c                   eval      §TRSSOCPRT = exSocieta
013100130114      *
013200130114     c                   endif
013300130114     c                   end
013400130114     C*
013500130114     c                   endsr
013600130114     C*---------------------------------------------------------------------------------
