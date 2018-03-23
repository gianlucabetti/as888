000001070424     H dftactgrp(*no) actgrp(*new)
000100970526     H*------------------------------------------------------------------------*
000200070403     H* Gestione versioni giri
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800070403     FFIDG11D   CF   E             WORKSTN INFDS(DEVDS1)
000900070403     F                                     SFILE(DG11D05:SFLULT)
001000070403     F                                     SFILE(DG11DMS:MSGULT)
001100970526     F*------------
001200070403     Ffigrv01l  IF   E           K DISK
001500970526     D*------------------------------------------------------------------------*
001600970526     D* SCHIERE
001700970526     D*------------------------------------------------------------------------*
001800970526     D*------------
001900970526     D* DECODIFICHE
002000970526     D*------------
002100970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* MESSAGGI
002400970526     D*------------
002500970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002600970528     D*------------
002700970528     D* COMANDI
002800970528     D*------------
002900970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* DESCRIZIONI OPZIONI
003200970526     D*------------
003300970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003400970526     D*------------
003500970526     D* DESCRIZIONI TASTI FUNZIONALI
003600970526     D*------------
003700970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003800970526     D*------------
003900970526     D* OPZIONI DA CARICARE A VIDEO
004000970526     D*------------
004100970526     D OPV             S             19    DIM(24)
004200970526     D*------------
004300970526     D* FUNZIONI DA CARICARE A VIDEO
004400970526     D*------------
004500970527     D FUV             S             19    DIM(24)
004600970526     D*------------------------------------------------------------------------*
004700970526     D* INPUT
004800970526     D*------------------------------------------------------------------------*
004900970526     D*------------
005000970526     D* RIDEFINIZIONE OPZIONI VIDEO
005100970526     D*------------
005200970526     D OPDDS           DS
005300970526     D  OPDOPZ                 1      2
005400970526     D  OPDDE1                 3     19
005500970526     D  OPDOP1                21     22
005600970526     D  OPDFI0                23     34
005700970526     D  OPDATT                35     35
005800970526     D  OPDFI1                36     80
005900970526     D  OPDDES                 1     19
006000970526     D  OPDRIG                 1     80
006100970526     D*------------
006200970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006300970526     D*------------
006400970526     D FUDDS           DS
006500970526     D  FUDDE1                 1      1
006600970526     D  FUDFUN                 2      3
006700970526     D  FUDDE2                 4     19
006800970526     D  FUDFI0                20     34
006900970526     D  FUDATT                35     35
007000970526     D  FUDFI1                36     80
007100970526     D  FUDDES                 1     19
007200970526     D  FUDRIG                 1     80
007300970526     D*------------
007400970526     D* DS GESTIONE TASTI FUNZIONALI
007500970526     D*------------
007600970526     D DSKEY         E DS
007700970526     D DEVDS1          DS
007800970526     D  KEY                  369    369
007900970526     D*------------
008000970526     D* DS "XMSG" - GESTIONE MESSAGGI
008100970526     D*------------
008200970526     D DSMSG         E DS
008300970526     D*------------
008400020916     D* DS REPERIMENTO DATI UTENTE
008500970526     D*------------
008600020916     D TIBS34DS      E DS                                                       *Profili utente
008700020916     D DDATIUTE      E DS                                                       *Dati utente
008800020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008801070403     D*------------
008802070403     D* DS DI PROCEDURA
008803070403     D*------------
008804070411     D fidg09ds      E DS                  inz
008805070411     D fidg11ds      E DS                  inz
010100970527     D*------------------
010200970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
010300970527     D*------------------
010400970527     D WLBDA8          DS                  INZ
010500970527     D  G08DAT                 1      8  0
010600970527     D  G08INV                 9     16  0
010700970527     D  G08ERR                17     17
010800970527     D  G08TGI                18     22  0
011400970526     D*------------
011500970526     D* ARCHITETTURA
011600970526     D*------------
011700970526     D KPJBA         E DS
011701070411     D*------------
011702070411     D* VARIABILI D WRK
011703070411     D*------------
011704070411     D savKPJBA        S                   like(KPJBA)
012500970526     C*------------------------------------------------------------------------*
012600970526     C* MAIN LINES
012700970526     C*------------------------------------------------------------------------*
012701070403     C*
012702070403     C* PRIMO POSIZIONAMENTO
012703070403     C                   EXSR      POSSFL                                       *RICARICA SFILE
013400970526     C*
013500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013600070403     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013700000703     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013900970526     C*
014000970526     C* REIMPOSTA ERRORE DI PROCERURA
014100970526     C                   MOVEL     '0'           ERRL00
014200970526     C*
014300970526     C* EMETTE IL SFILE
014400970526     C                   EXSR      OUTSFL
014500970526     C*
014600970526     C* GESTIONE TASTI FUNZIONALI
014700970526     C                   EXSR      FUNCON
014800970526     C*
014900070403     C                   ENDDO
014901070403     C*
014902070403     C* RITORNO PARAMETRI
014903070403     C                   MOVEL(P)  fidg11ds      KPJBU
015501070403     C*
015800970526     C* FINE PGM
015900970526     C                   SETON                                        LR
016000970526     C*------------------------------------------------------------------------*
016100970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
016200970526     C*------------------------------------------------------------------------*
016300970526     C     F03GES        BEGSR
016400970526     C*
016500970526     C* IMPOSTA "PREMUTO F03"
016600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016601070403     C                   MOVEL     '1'           d11of03
016800970526     C*
016900970526     C                   ENDSR
017000970526     C*------------------------------------------------------------------------*
017100970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
017200970526     C*------------------------------------------------------------------------*
017300970526     C     F05GES        BEGSR
017400970526     C*
017500970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017600970526     C*
017700970526     C                   ENDSR
017800970526     C*------------------------------------------------------------------------*
017900070403     C* F06GES - GESTIONE F06 -> INSERIMENTO
018000970526     C*------------------------------------------------------------------------*
018100970526     C     F06GES        BEGSR
018800970526     C*
018900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
018901070403     C                   Z-ADD     *ZEROS        d11ifgs
019200070403     C                   Z-ADD     *ZEROS        d11idde
019300970602     C                   MOVEL     DEPOP0        WOP0
019400970529     C                   MOVEL     'F06'         WOP1
019600970526     C                   EXSR      LANPGM                                       *LANCIA PGM
019700970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019800970526E   1C                   ENDIF
019900970526     C*
020000970526     C                   ENDSR
020100970526     C*------------------------------------------------------------------------*
020200970526     C* F12GES - GESTIONE F12 -> RITORNO
020300970526     C*------------------------------------------------------------------------*
020400970526     C     F12GES        BEGSR
020500970526     C*
020600970526     C* IMPOSTA "PREMUTO F12"
020700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020701070403     C                   MOVEL     '1'           d11of12
020900970526     C*
021000970526     C                   ENDSR
023700970526     C*------------------------------------------------------------------------*
023800970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
023900970526     C*------------------------------------------------------------------------*
024000970526     C     F23GES        BEGSR
024100970526     C*
024200970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
024300970526     C*
024400970526     C                   ENDSR
024500970526     C*------------------------------------------------------------------------*
024600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024700970526     C*------------------------------------------------------------------------*
024800970526     C     F24GES        BEGSR
024900970526     C*
025000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025100970526     C*
025200970526     C                   ENDSR
025300970526     C*------------------------------------------------------------------------*
025400970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
025500970526     C*------------------------------------------------------------------------*
025600970526     C     RUPGES        BEGSR
025700970526     C*
025800970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
025900970526     C*
026000970526     C                   ENDSR
026100970526     C*------------------------------------------------------------------------*
026200070403     C* ENTGES - GESTIONE ENTER -> INVIO
026300970526     C*------------------------------------------------------------------------*
026400970526     C     ENTGES        BEGSR
026401070403     C*
026402070403     C* AL MOMENTO UNICAMENTE IN GESTIONE A PERSONALE EDP
026404070413     C                   IF        %subst(KNMUS:1:3) = 'EDP' OR
026405070413     C                             DEPOP0            = 'P01'
026406070403     C                   MOVEL     'S'           wAutPO            1
026407070403     C                   ELSE
026408070403     C                   MOVEL     'N'           wAutPO
026409070403     C                   MOVEL     '1'           ERRL00
026410070403     C                   MOVEL     MSG(8)        DSMSMS
026411070403     C                   ENDIF
026412070124     C*
026413070124     C* CONTROLLO CHE IL P.O. INDICATO IN SELEZIONE SIA EFFETTIVAMENTE IN GESTIONE
026414070124     C* ALL'UTENTE CORRENTE
026415070403     C*                  MOVEL     'S'           wAutPO            1
026416070403     C*                  IF        DEPOP0 <> '001'  AND
026417070403     C*                            DEPOP0 <> 'P01'  AND
026418070403     C*                            VI1FGS <> *ZEROS AND
026419070403     C*                            %subst(KNMUS:1:3) <> 'EDP'
026420070403     C*                  CLEAR                   FNLV50DS
026421070403     C*                  EVAL      D50TLA = 'L'
026422070403     C*                  EVAL      D50PRU = KNMUS
026423070403     C*                  EVAL      D50FGS = VI1FGS
026424070403     C*                  CALL      'FNLV50R'
026425070403     C*                  PARM                    FNLV50DS
026426070403     C*                  IF        D50ERR <> *BLANKS
026427070403     C*                  MOVEL     'N'           wAutPO
026428070403     C*                  MOVEL     '1'           ERRL00
026429070403     C*                  MOVEL     D50MSG        DSMSMS
026430070403     C*                  ENDIF
026431070403     C*                  ENDIF
026432070124     C*
026433070124     C                   IF        wAutPO = 'S'
026500000703     C*
026600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
026700970526     C                   EXSR      NEWPOS
026800970526     C*
026900970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
027000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027100970526IF  2C     SWPOSI        IFNE      0
027200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
027300970526X   2C                   ELSE
027400970526IF  3C     SFLULT        IFNE      0
027500970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
027600970526     C*
027700970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
027800970526IF  4C     SWMODI        IFNE      0
027900970526     C                   EXSR      POSSFL
028000970526E   4C                   ENDIF
028100970526E   3C                   ENDIF
028200970526E   2C                   ENDIF
028300970526E   1C                   ENDIF
028400970526     C*
028401070124     C                   ENDIF
028402070124     C*
028500970526     C                   ENDSR
028600000703     C*------------------------------------------------------------------------*
028700970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
028800970526     C*------------------------------------------------------------------------*
028900970526     C     O01GES        BEGSR
029000970526     C*
029100970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
029101070403     C                   Z-ADD     VIDFGS        d11ofgs
029102070403     C                   Z-ADD     VIDDDE        d11odde
029103070403     C                   Z-ADD     VIHDDE        d11oddei
029104070403     C                   Z-ADD     VIDDSCAD      d11odsc
029105070403     C                   Z-ADD     VIDDSCAD      d11odsci
029301070403     C                   MOVEL     VIDDES        d11odes
029306070403     C                   MOVEL     VIHFLO        d11oflo
029500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
029600970526     C*
029700970526     C                   ENDSR
029800970526     C*------------------------------------------------------------------------*
029900970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
030000970526     C*------------------------------------------------------------------------*
030100970526     C     O02GES        BEGSR
030200970526     C*
030800970527     C* MODIFICA
030900970526IF  1C     ERRL00        IFEQ      '0'
030901070403     C                   Z-ADD     VIDFGS        d11ifgs
031200070403     C                   Z-ADD     VIHDDE        d11idde
031300970602     C                   MOVEL     DEPOP0        WOP0
031400970526     C                   MOVEL     'O02'         WOP1
031600970526     C                   EXSR      LANPGM
031700070123     C                   Z-ADD     1             swmodi
031800970526E   1C                   ENDIF
031900970526     C*
032000970526     C                   ENDSR
032100991014     C*------------------------------------------------------------------------*
032200070403     C* O03GES - GESTIONE OPZIONE 3=COPIA
032300991014     C*------------------------------------------------------------------------*
032400991014     C     O03GES        BEGSR
032500991014     C*
032700070119     C* COPIA
032800991014IF  1C     ERRL00        IFEQ      '0'
032801070403     C                   Z-ADD     VIDFGS        d11ifgs
033100070403     C                   Z-ADD     VIHDDE        d11idde
033200991014     C                   MOVEL     DEPOP0        WOP0
033300991014     C                   MOVEL     'O03'         WOP1
033500991014     C                   EXSR      LANPGM
033600070123     C                   Z-ADD     1             swmodi
033700991014E   1C                   ENDIF
033800991014     C*
033900991014     C                   ENDSR
035900970526     C*------------------------------------------------------------------------*
036000970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
036100970526     C*------------------------------------------------------------------------*
036200970526     C     O05GES        BEGSR
036300970526     C*
036301070403     C                   Z-ADD     VIDFGS        d11ifgs
036600070403     C                   Z-ADD     VIHDDE        d11idde
036700970602     C                   MOVEL     DEPOP0        WOP0
036800970526     C                   MOVEL     'O05'         WOP1
037000970526     C                   EXSR      LANPGM
037100970526     C*
037200970526     C                   ENDSR
037201070411     C*------------------------------------------------------------------------*
037202070411     C* O10GES - GESTIONE OPZIONE 10=ANAGRAFICA GIRI
037203070411     C*------------------------------------------------------------------------*
037204070411     C     O10GES        BEGSR
037205070411     C*
037206070411     C                   eval      savKPJBA = KPJBA
037207070411     C                   clear                   fidg09ds
037208070411     C                   eval      d09iop0 = d11iop0
037209070411     C                   move(p)   VIDFGS        d09ifgs
037210070411     C                   z-add     VIHDDE        d09idde
037211070411     C                   eval      kpjbu = fidg09ds
037212070411     C                   call      'FIDG09R'
037213070411     C                   parm                    kpjba
037214070411     C                   eval      KPJBA = savKPJBA
037215070411     C*
037216070411     C                   ENDSR
044000970526     C*------------------------------------------------------------------------*
044100970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
044200970526     C*------------------------------------------------------------------------*
044300970526     C     NEWPOS        BEGSR
044400970526     C*
044500970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
044600970526     C*         1= RICHIESTO POSIZIONAMENTO
044700970526     C                   Z-ADD     0             SWPOSI            7 0
044800970526     C*
044900970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
045000970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
045100970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
045200970526     C                   Z-ADD     0             SWMODI            1 0
045700970526     C*
045800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
045801070119IF  1C     VI1FGS        IFNE      depFGS
046500070119     C     VI1DES        ORNE      depDES
046501070119     C     VI1DAT        ORNE      depDAT
046800970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
046900970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
047000970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
047001070119     C                   Z-ADD     VI1FGS        depFGS                         *DEPOSITI
047500070119     C                   MOVEL     VI1DES        depDES                         *DEPOSITI
047900070119     C                   MOVEL     VI1DAT        depDAT                         *DEPOSITI
048000970526E   2C                   ENDIF
048100970526E   1C                   ENDIF
048200970526     C*
048300970526     C                   ENDSR
048400970526     C*------------------------------------------------------------------------*
048500970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
048600970526     C*------------------------------------------------------------------------*
048700970528     C     SETFIL        BEGSR
049401070119     C*
049402070403IF  1C     WTIPRI        IFEQ      'DFT'                                        *PER DESCRIZ. GIRO
049403070403     C     *loval        SETLL     figrv01l
049404070119E   1C                   ENDIF
049500980107     C*
049600970526     C* LETTURA RECORD
049700970526IF  1C     *IN99         IFEQ      *OFF
049800970526     C                   EXSR      LETFIL
049900970526E   1C                   ENDIF
050000970526     C*
050100970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
050200970526IF  1C     *IN99         IFEQ      *ON
050300970526     C                   Z-ADD     1             SFLEOF
050400970526E   1C                   ENDIF
050500970526     C*
050600970526     C                   ENDSR
050700970526     C*------------------------------------------------------------------------*
050800970526     C* LETFIL - LETTURA RECORD
050900970526     C*------------------------------------------------------------------------*
051000970526     C     LETFIL        BEGSR
051100970526     C*
051200970526     C* LEGGE FINO A:
051300970526     C*     - FINE FILE
051400970526     C*     - TROVATO RECORD VALIDO
051500970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
051600010306     C     *IN99         DOWEQ     *OFF
051700970526     C     WRECOK        ANDEQ     'N'
051800970526     C*
051900970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
052301070403     C     WTIPRI        IFEQ      'DFT'                                        *PER DESCRIZ. GIRO
052302070403     C                   READ      figrv01l                               99
052303070119     C                   ENDIF
052304070119     C*
052400970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
052500010306     C     *IN99         IFEQ      *ON                                          *FINE FILE
052600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
052700010306     C                   ELSE
052800970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
052900010306     C                   ENDIF
053000010306     C                   ENDDO
053100970526     C*
053200970526     C                   ENDSR
053300970526     C*------------------------------------------------------------------------*
053400070403     C* CHKREC - CONTROLLA VALIDITA' RECORD
053500970526     C*------------------------------------------------------------------------*
053600970526     C     CHKREC        BEGSR
053700970526     C*
053800970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
053900010117     C*
054000070403     C* FILTRO X FILIALE GESTIONE
054100010427     C                   if        WRECOK = 'S'
054200070119     C     VI1FGS        ifeq      *zeros
054300010117     C                   else
054400070403     C                   if        grvfgs <> VI1FGS
054700010117     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
056100010117     C                   endif
056101070119     C                   endif
056200010427     C                   endif
056211070119     C*
056212070119     C* FILTRO X ALLA DATA
056213070119     C                   if        WRECOK = 'S'
056214070123     C     VI1DAT        ifeq      *zeros
056215070123     C                   else
056218070123     C                   z-add     VI1DAT        G08DAT
056219070123     C                   z-add     *ZEROS        G08INV
056220070123     C                   movel     '0'           G08ERR
056221070123     C                   call      'XSRDA8'
056222070123     C                   parm                    WLBDA8
056224070123     C                   z-add     G08INV        wvi1dat
056232070123     C*
056233070403     C                   if        d11idda > *zeros
056234070403     C                   if        grvdscad >= d11idda
056236070403     C                   else
056237070403     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
056238070403     C                   endif
056239070403     C*
056242070403     C                   else
056243070403     C                   if        wVI1DAT >= grvdde AND
056244070403     C                             wVI1DAT <= grvdscad
056245070119     C                   else
056246070119     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
056247070403     C                   endif
056248070119     C                   endif
056249070119     C                   endif
056250070119     C                   endif
060800010427     C*
060900070119     C* FILTRA X DESCRIZIONE GIRO (IN "LIKE")
061000010427     C                   if        WRECOK = 'S'
061300070119     C     VI1DES        ifeq      *blanks
061400010709     C                   else
061700010709     C                   z-add     *zeros        pos               2 0
061800070403     C                   eval      pos = %scan(%trim(VI1DES):grvdes)
061900010709     C                   if        pos = *zeros
062000010427     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
062100010427     C                   endif
062200010427     C                   endif
062300010427     C                   endif
064600970526     C*
064700970526     C                   ENDSR
067700970526     C*------------------------------------------------------------------------*
067800070403     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
067900970526     C*------------------------------------------------------------------------*
068000970526     C     IRESFL        BEGSR
068100970526     C*
068200070119     C                   movel     *BLANKS       OPZKEY
068400070403     C                   movel     grvdes        VIDDES
068401070403     C                   movel     grvkey        VIDKEY
068503070403     C                   movel     grvflo        VIHFLO
068504070403     C                   z-add     grvdde        VIHDDE
068700070403     C                   z-add     grvdscad      VIHDSCAD
068800070403     C                   z-add     grvfgs        VIDFGS
068807070119     C*
068808070119     C* Inverto a video data decorrenza
068900070403     C                   Z-ADD     grvdde        G08INV
069000991025     C                   Z-ADD     *ZEROS        G08DAT
069100991025     C                   MOVEL     '3'           G08ERR
069200991025     C                   CALL      'XSRDA8'
069300991025     C                   PARM                    WLBDA8
069400070119     C                   Z-ADD     G08DAT        VIDDDE
069401070119     C*
069402070119     C* Inverto a video data scadenza
069500070403     C                   Z-ADD     grvdscad      G08INV
069600991025     C                   Z-ADD     *ZEROS        G08DAT
069700991025     C                   MOVEL     '3'           G08ERR
069800991025     C                   CALL      'XSRDA8'
069900991025     C                   PARM                    WLBDA8
070000070124     C                   Z-ADD     G08DAT        VIDDSCAD
071500011129     C*
071600970526     C                   ENDSR
071700970526     C*------------------------------------------------------------------------*
071800970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
071900970526     C*------------------------------------------------------------------------*
072000070119     C     CHKPAR        BEGSR
073000970528     C*
077800000801     C*
077900970526     C                   ENDSR
084200970526     C*------------------------------------------------------------------------*
084300970526     C* LANPGM - LANCIA UN PROGRAMMA
084400970526     C*------------------------------------------------------------------------*
084500970526     C     LANPGM        BEGSR
084600970526     C*
084700970526     C* IMPOSTA LA DS DI PROCEDURA
084900070403     C                   MOVEL     WOP0          d11IOP0                        *LIVELLO PROCEDURA
085000070403     C                   MOVEL     WOP1          d11IOP1                        *LIVELLO PGM
085100070403     C                   MOVEL     '0'           d11OF03                        *PREMUTO F03=FINE
085200070403     C                   MOVEL     '0'           d11OF12                        *PREMUTO F12=RITORNO
085300070403     C                   MOVEL     '0'           d11OERR                        *'1'=ERRORE
085400070403     C                   MOVEL     *BLANKS       d11OMSG                        *MESSAGGIO DI RITORN
085800070403     C                   MOVEL(P)  fiDG11ds      KPJBU
087300000705     C*
087900160401     C                   CALL      'FIDG12R'
088000970612     C                   PARM                    KPJBA
088001070403     C                   MOVEL(P)  KPJBU         fiDG11ds
088200970526     C*
088300970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
088500070403IF  1C     d11OF03       IFEQ      '1'                                          *PREMUTO F03
088600070122     C                   MOVEL     '1'           ERRL00                         *ERRORE
088700970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
088800970526X   1C                   ELSE                                                   *NON PREMUTO F03
088900070403     C                   MOVEL     d11OMSG       DSMSMS                        *MESSAGGIO DI RITORN
089000970526E   1C                   ENDIF
089900970526     C*
090000970526     C                   ENDSR
090100970526     C*------------------------------------------------------------------------*
090200070119     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
090300970526     C*------------------------------------------------------------------------*
090400970526     C     CHKFOP        BEGSR
090500970626     C*
090600970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
090700970626IF  1C     DEPOP0        IFEQ      'P01'
090800970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
090900970626     C                   MOVEL     'S'           OPDATT
091000970626     C                   MOVEL     OPDRIG        OPD(1)
091100970626X   1C                   ELSE
091200970626     C* OPZIONI
091300970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
091400970626     C                   MOVEL     'S'           OPDATT
091500970626     C                   MOVEL     OPDRIG        OPD(5)
091501070411     C                   MOVEL     OPD(10)       OPDRIG                         *ANAGRAFICA GIRI
091502070411     C                   MOVEL     'S'           OPDATT
091503070411     C                   MOVEL     OPDRIG        OPD(10)
092500970626     C* FUNZIONI
092600970626E   1C                   ENDIF
092700970626     C*
092800970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
092900070122IF  1C     DEPOP0        IFEQ      'P03'
092901070122     C     DEPOP0        OREQ      'P04'
093100970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
093200970626     C                   MOVEL     'S'           OPDATT
093300970626     C                   MOVEL     OPDRIG        OPD(2)
093400070403     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
093500070403     C                   MOVEL     'S'           OPDATT
093600070403     C                   MOVEL     OPDRIG        OPD(3)
093700070403     C***                MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
093800070403     C***                MOVEL     'S'           OPDATT
093900070403     C***                MOVEL     OPDRIG        OPD(4)
094000070403     C***                MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
094100070403     C***                MOVEL     'S'           OPDATT
094200070403     C***                MOVEL     OPDRIG        OPD(7)
094300970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
094400970626     C                   MOVEL     'S'           FUDATT
094500970626     C                   MOVEL     FUDRIG        FUD(6)
094900970626E   1C                   ENDIF
094901070119     C*
094902070124     C* SE IN SCELTA INOLTRE DISABILITO ALCUNE SELEZIONI A VIDEO
094903070124     C                   SETON                                        70
094904070124IF  1C     DEPOP0        IFEQ      'P01'
094905070124     C                   SETOFF                                       70
094906070124     C                   SETOFF                                       93
094907070119E   1C                   ENDIF
095000970526     C*
095100970526     C* GESTIONE OPZIONI A VIDEO
095200970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
095300970526     C                   EXSR      OPZGES
095400970526     C*
095500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
095600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
095700970526     C                   EXSR      FUNGES
095800970526     C*
095900970526     C                   ENDSR
098100020916     C*--------------------------------------------------------------------------------------------*
098200070119     C* REPERISCE I DATI UTENTE
098300020916     C*--------------------------------------------------------------------------------------------*
098400020916     C     REPDATIUTE    BEGSR
098500020916     C*
098600020916     C* INIZIALIZZA VARIABILI DI WRK
098700020916     C                   CLEAR                   TIBS34DS
098800020916     C                   CLEAR                   AZUTEDS
098900020916     C                   CLEAR                   DDATIUTE
099000020916     C*
099100020916     C     *DTAARA       DEFINE    §azute        azuteds
099200020916     C     *DTAARA       DEFINE    §datiute      ddatiute
099300020916     C                   IN(E)     *DTAARA
099400020916if  1C                   IF        %Error
099500020916     c                   EVAL      I34Tla = 'L'
099600020916     C                   CALL      'TIBS34R'
099700020916     C                   PARM                    Tibs34Ds
099800020916     C                   IN        *DTAARA
099900020916e   1C                   ENDIF
100000020916     C*
100100020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
100200020916     C                   MOVEL(P)  RSUT          DSFIRS           20
100300020916     C*
100400020916     C                   ENDSR
100500970526     C*------------------------------------------------------------------------*
100600970526     C* *INZSR - OPERAZIONI INIZIALI
100700970526     C*------------------------------------------------------------------------*
100800970526     C     *INZSR        BEGSR
100900970526     C*---
101000970526     C* RICEVIMENTO PARAMETRI
101100970526     C*---
101200970526     C     *ENTRY        PLIST
101300970612     C                   PARM                    KPJBA
101301070306     C*
101302070306     C* REPERIMENTO DATI UTENTE
101303070306     C                   EXSR      REPDATIUTE
101304070122     C*
101305070125     C                   IF        %subst(KNMUS:1:3) = 'EDP'
101306070125     C                   SETON                                        93
101307070125     C                   ELSE
101308070124     C                   SETOFF                                       93
101309070125     C                   ENDIF
101310070124     C*
101311070122     C                   IF        KPJBU <> *blanks
101400070403     C                   MOVEL     KPJBU         fiDG11ds
101401070122     C*
101402070122     C* DEPOSITA LE OPZIONI DI ENTRATA
101403070403     C                   MOVEL     d11IOP0       DEPOP0                         *LIVELLO PROCEDURA
101404070403     C                   MOVEL     d11IOP1       DEPOP1                         *LIVELLO PROGRAMMA
101405070122     C*
101406070122     C* IMPOSTA I PARAMETRI DI INGRESSO
101407070403     C                   MOVE(P)   d11ifgs       VI1FGS
101408070403     C                   MOVE(P)   d11ifgs       depFGS
101410070403     C                   Z-ADD     d11idda       wVI1DDA
101412070403     C                   Z-ADD     d11idat       wVI1DAT
101414070403     C                   MOVEL(P)  d11ides       VI1DES
101415070403     C                   MOVEL(P)  d11ides       depDES
101419070122     C*
101420070122     C* Inverto la data "validi al"
101421070403     C                   IF        wVI1DDA > *zeros
101422070403     C                   Z-ADD     wVI1DDA       G08INV
101423070403     C                   ELSE
101424070403     C                   Z-ADD     wVI1DAT       G08INV
101425070403     C                   ENDIF
101427070122     C                   Z-ADD     *ZEROS        G08DAT
101428070122     C                   MOVEL     '3'           G08ERR
101429070122     C                   CALL      'XSRDA8'
101430070122     C                   PARM                    WLBDA8
101431070122     C                   Z-ADD     G08DAT        VI1DAT
101432070124     C                   Z-ADD     G08DAT        depDAT
101433070122     C*
101434070122     C                   EXSR      CHKPAR
101435070122IF  1C     ERRL00        IFEQ      '1'
101436070403     C                   MOVEL     '1'           d11OERR
101437070403     C                   MOVEL     DSMSMS        d11OMSG
101438070122E   1C                   ENDIF
101439070122     C*
101440070122     C* SE ENTRATA DA MENU' GESTISCO IN BASE ALL'UTENTE
101441070122     C                   ELSE
101442070403     C                   CLEAR                   fiDG11ds
101443070122     C                   IF        %subst(KNMUS:1:3) = 'EDP'
101444070122     C                   EVAL      DEPOP0 = 'P04'
101445070122     C                   ELSE
101446070306     C                   EVAL      VI1FGS = DUTPOU
101447070124     C                   EVAL      DEPOP0 = 'P03'
101448070122     C                   ENDIF
101449070122     C                   ENDIF
101500970526     C*
101600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
101700970526     C     XMSGPA        PLIST
101800970526     C                   PARM                    DSMSMS
101900970526     C                   PARM                    DSMSPG
102000970526     C                   PARM                    DSMSKE
102100970526     C                   PARM                    DSMSER
102200970526     C*
102300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
102400970526     C                   CALL      'XKEY'
102500970526     C                   PARM                    DSKEY
102501070119     C*
102502070119     C* DI DEFAULT IMPOSTO L?ORDINAMENTO X CODICE GIRO
102503070403     C                   MOVEL     'DFT'         WTIPRI            3
102600970526     C*---
102700970526     C* VARIABILI NON RIFERITE AL DATA BASE
102800970526     C*---
102900970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
103000970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
103100970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
103200970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
103300970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
103400970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
103500970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
103600970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
103700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
103800070403     C     *LIKE         DEFINE    d11IOP0       DEPOP0                         *DI DEPOSITO
103900070403     C     *LIKE         DEFINE    d11IOP1       DEPOP1
104000070403     C     *LIKE         DEFINE    d11IOP0       WOP0                           *DI LAVORO
104100070403     C     *LIKE         DEFINE    d11IOP1       WOP1
104500970526     C*---
105400070403     C     *LIKE         DEFINE    d11ides       DepDES
105700070403     C     *LIKE         DEFINE    d11idat       DepDAT
105701070403     C     *LIKE         DEFINE    d11idat       wVI1DAT
105702070403     C     *LIKE         DEFINE    d11idda       wVI1DDA
105703070403     C     *LIKE         DEFINE    d11ifgs       DepFGS
105800001020     C*---
105900970526     C* VARIABILI DI CONTROLLO
106000970526     C*---
106100970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
106200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
106300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
106400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
106500970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
106600070201     C                   Z-ADD     11            SFLPAG                         *N°RIGHE X PAG
106700970526     C*
106800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
106900070403     C                   MOVEL     'FIDG11R'     DSMSPG
107000070403     C                   MOVEL     'FIDG11R'     MSGQUE
107100970526     C                   MOVEL     *BLANKS       MSGKEY
107200970526     C                   MOVEL     *BLANKS       DSMSMS
107300970526     C                   MOVEL     *BLANKS       DSMSKE
107400970526     C                   Z-ADD     *ZEROS        DSMSER
111800970526     C*
111900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
112000970526     C                   EXSR      CHKFOP
112100970526     C*
112200970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
112300970530IF  1C     DEPOP0        IFEQ      'P01'
112400070119     C                   MOVEL     DEC(1)        VIDING
112500970530E   1C                   ENDIF
112600970530IF  1C     DEPOP0        IFEQ      'P02'
112700070119     C                   MOVEL     DEC(2)        VIDING
112800970530E   1C                   ENDIF
112801070119IF  1C     DEPOP0        IFEQ      'P03'
112802070119     C                   MOVEL     DEC(3)        VIDING
112803070119E   1C                   ENDIF
112900070122IF  1C     DEPOP0        IFEQ      'P04'
113000070122     C                   MOVEL     DEC(4)        VIDING
113100970526E   1C                   ENDIF
113200970526     C*
113300070119     C                   ENDSR
113400970526     C*------------------------------------------------------------------------*
113500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
113700970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
113800970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
113900970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
114000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
114100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
114200970526     C*------------------------------------------------------------------------*
114300970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
114400970526     C*------------------------------------------------------------------------*
114500970526     C     POSSFL        BEGSR
114600970526     C*
114700970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
114800970526     C                   EXSR      CLRSFL
114900970526     C*
115000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
115100970526     C                   EXSR      SETFIL
115200970526     C*
115300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
115400970526     C     SFLEOF        IFEQ      0
115500970526     C                   EXSR      CARSFL
115600970526     C                   END
115700970526     C*
115800970526     C                   ENDSR
115900970526     C*------------------------------------------------------------------------*
116000970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
116100970526     C*------------------------------------------------------------------------*
116200970526     C     ENTATT        BEGSR
116300970526     C*
116400970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
116500970526     C                   Z-ADD     0             OPZOK             1 0
116600970526     C*
116700970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
116800970526     C                   Z-ADD     0             SFLVAR            5 0
116900970526     C*
117000970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
117100970526     C                   Z-ADD     0             SFLERR
117200970526     C*
117300970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
117400970526     C                   Z-ADD     SFLULT        WFLULT
117500970526     C*
117600970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
117700070403     C                   READC     DG11D05                                98
117800970526     C     *IN98         DOWEQ     '0'
117900970526     C*
118000970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
118100970526     C                   ADD       1             SFLVAR
118200970526     C*
118300970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
118400970526     C     OPZOK         IFEQ      0
118500970526     C*
118600970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
118700970526     C     OPZKEY        IFNE      *BLANKS
118800970526     C                   EXSR      OPZCON
118900970526     C                   END
119000970526     C*
119100970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
119200970526     C*            2. PULIZIA SCELTA IN SFL
119300970526     C     OPZOK         IFEQ      0
119400970526     C                   MOVEL     '0'           *IN41
119500970526     C                   MOVEL     *BLANKS       OPZKEY
119600970526     C                   ELSE
119700970526     C*
119800970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
119900970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
120000970526     C                   MOVEL     '1'           *IN41
120100970526     C                   Z-ADD     SFLULT        SFLERR
120200970526     C                   END
120300970526     C                   ELSE
120400970526     C*
120500970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
120600970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
120700970526     C                   MOVEL     '1'           *IN41
120800970526     C                   END                                                    OPZOK=0
120900970526     C*
121000970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
121100970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
121200970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
121300970526     C     OPZOK         IFNE      0
121400970526     C                   Z-ADD     SFLERR        SFLNBR
121500970526     C                   ELSE
121600970526     C                   Z-ADD     SFLULT        SFLNBR
121700970526     C                   END
121800970526     C*
121900970526     C* AGGIORNO RECORD SFL
122000070403     C                   UPDATE    DG11D05
122100970526     C*
122200070403     C                   READC     DG11D05                                98
122300970526     C                   END                                                    DO WHILE
122400970526     C*
122500970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
122600970526     C                   Z-ADD     WFLULT        SFLULT
122700970526     C*
122800970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
122900970526     C     SFLVAR        IFEQ      0
123000970526     C*---                 MOVEL'1'       FINL00
123100970526     C                   END
123200970526     C*
123300970526     C                   ENDSR
123400970526     C*------------------------------------------------------------------------*
123500970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
123600970526     C*------------------------------------------------------------------------*
123700970526     C     CLRSFL        BEGSR
123800970526     C*
123900970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
124000970526     C* (0=NON FINE FILE  1=FINE FILE)
124100970526     C                   Z-ADD     0             SFLEOF
124200970526     C*
124300970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
124400970526     C                   Z-ADD     0             SFLULT
124500970526     C*
124600970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
124700970526     C                   MOVEL     '1'           *IN40
124800070403     C                   WRITE     DG11D06
124900970526     C                   MOVEL     '0'           *IN40
125000970526     C*
125100970526     C                   ENDSR
125200970526     C*------------------------------------------------------------------------*
125300970526     C* CARSFL - CARICAMENTO SFL
125400970526     C*------------------------------------------------------------------------*
125500970526     C     CARSFL        BEGSR
125600970526     C*
125700970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
125800970526     C     SFLULT        IFEQ      0
125900970526     C                   EXSR      SETFIL
126000970526     C                   END
126100970526     C*
126200970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
126300970526     C                   Z-ADD     SFLULT        WFLULT
126400970526     C*
126500970526     C* CICLO:
126600970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
126700970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
126800970526     C                   Z-ADD     1             SFLRIG
127002070403     C                   DOW       SFLRIG <= SFLPAG AND
127003070205     C                             SFLEOF  = *ZEROS
127100970526     C*
127200970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
127300970526     C                   MOVEL     *BLANKS       OPZKEY
127400970526     C*
127500970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
127600070124     C                   EXSR      IRESFL
127700970526     C*
127800970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
127900970526     C                   ADD       1             SFLULT
128000970526     C*
128100970526     C* SCRITTURA RECORD SFL
128200070403     C                   WRITE     DG11D05
128300970526     C                   ADD       1             SFLRIG
128400970526     C*
128500970526     C* LETTURA FILE DA CARICARE
128600970526     C                   EXSR      LETFIL
128700970526     C                   END                                                    DO WHILE
128800970526     C*
128900970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
129000970526     C     WFLULT        IFNE      SFLULT
129100970526     C                   Z-ADD     WFLULT        SFLNBR
129200970526     C                   ADD       1             SFLNBR
129300970526     C                   END
129400970526     C*
129500970526     C                   ENDSR
129600970526     C*------------------------------------------------------------------------*
129700970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
129800970526     C*------------------------------------------------------------------------*
129900970526     C     OUTSFL        BEGSR
130000970526     C*
130100970526     C* WRITE RECORD TESTATA VIDEO
130200070403     C                   WRITE     DG11D01
130300970526     C*
130400970526     C* VISUALIZZAZIONE MESSAGGIO
130500970526     C                   EXSR      SNDMSG
130600970526     C*
130700970526     C* WRITE RECORD OPZIONI VIDEO
130800070403     C                   WRITE     DG11D02
130900970526     C*
131000970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
131100070403     C                   WRITE     DG11D03
131200970526     C*
131300970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
131400070403     C                   WRITE     DG11D04
131500970526     C*
131600970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
131700970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
131800970526     C     SFLEOF        IFEQ      0
131900970526     C                   MOVEL     '0'           *IN42
132000970526     C                   ELSE
132100970526     C*
132200970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
132300970526     C                   MOVEL     '1'           *IN42
132400970526     C                   END
132500970526     C*
132600970526     C* WRITE RECORD TASTI FUNZIONALI
132700070403     C                   WRITE     DG11D09
132800970526     C*
132900970526     C* CASO DI SFL VUOTO
133000970526     C     SFLULT        IFLE      0
133100970526     C*
133200970526     C*    - WRITE RECORD VUOTO
133300070403     C                   WRITE     DG11D0A
133400070403     C                   READ      DG11D0A                                99
133401070323     C*    - TRACCIO NN TROVATO NESSUN DATO RISPONDENTE A RICHIESTE
133402070403     C                   MOVEL     '1'           d11OERR
133403070403     C                   MOVEL     MSG(6)        d11OMSG
133500970526     C*
133600970526     C                   ELSE
133700970526     C*
133800970526     C* CASO DI SFL CON RECORDS
133900970526     C*
134000970526     C*    - WRITE RECORD CONTROLLO SFL
134100070403     C                   WRITE     DG11D06
134200070403     C                   READ      DG11D06                                99
134300970526     C                   END
134400970526     C*
134500970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
134600070403     C                   READ      DG11D03                                99
134700970526     C*
134800970526     C                   ENDSR
134900970526     C*------------------------------------------------------------------------*
135000970526     C* SNDMSG - MANDA MESSAGGIO
135100970526     C*------------------------------------------------------------------------*
135200970526     C     SNDMSG        BEGSR
135300970526     C*
135400970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
135500970526     C                   MOVEL     '1'           *IN40
135600070403     C                   WRITE     DG11DMC
135700970526     C                   MOVEL     '0'           *IN40
135800970526     C*
135900970526     C* VISUALIZZAZIONE MESSAGGIO
136000970526     C                   Z-ADD     0             MSGULT
136100970526     C                   CALL      'XMSG'        XMSGPA
136200970526     C     DSMSER        IFEQ      0
136300970526     C                   MOVEL     DSMSKE        MSGKEY
136400970526     C                   END
136500970526     C                   Z-ADD     1             MSGULT
136600070403     C                   WRITE     DG11DMS                                      SFL MESS
136700070403     C                   WRITE     DG11DMC                                      CONTR MESS
136800970526     C*
136900970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
137000070123     C*****              MOVEL     *BLANKS       DSMSMS
137100970526     C*
137200970526     C                   ENDSR
137300970526     C*------------------------------------------------------------------------*
137400970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
137500970526     C*------------------------------------------------------------------------*
137600970526     C     FUNCON        BEGSR
137700970526     C*
137800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
137900970526     C                   EXSR      FUNVAR
138000970526     C*
138100970526     C* PULISCO CAMPI DI LAVORO
138200970526     C                   Z-ADD     0             I                 5 0
138300970526     C                   Z-ADD     0             W                 5 0
138400970526     C*
138500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
138600970526     C     FUNKEY        IFNE      *BLANKS
138700970526     C     1             DO        24            I
138800970526     C                   MOVEL     FUD(I)        FUDRIG
138900970526     C*
139000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
139100970526     C     FUNKEY        IFEQ      FUDFUN
139200970526     C     FUDATT        ANDEQ     'S'
139300970526     C                   Z-ADD     I             W
139400970526     C                   END
139500970526     C                   END                                                    DO WHILE
139600970526     C*
139700970526     C* FUNZIONE NON ATTIVA (ERRORE)
139800970526     C     W             IFEQ      0
139900970526     C*
140000970526     C* EMISSIONE MESSAGGIO DI ERRORE
140100970526     C                   MOVEL     MSG(2)        DSMSMS
140200970526     C                   END                                                    W=0
140300970526     C                   END                                                    FUNKEY NE *BLANKS
140400970526     C*
140500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
140600970526     C     W             IFNE      0
140700970526     C     FUNKEY        OREQ      *BLANKS
140800970526     C*
140900970526     C     FUNKEY        IFEQ      *BLANKS
141000970526     C                   EXSR      ENTGES
141100970526     C                   END
141200970526     C     FUNKEY        IFEQ      '03'
141300970526     C                   EXSR      F03GES
141400970526     C                   END
141500970526     C     FUNKEY        IFEQ      '05'
141600970526     C                   EXSR      F05GES
141700970526     C                   END
141800970526     C     FUNKEY        IFEQ      '06'
141900970526     C                   EXSR      F06GES
142000970526     C                   END
142001070202     C     FUNKEY        IFEQ      '08'
142003070202     C                   END
142004070119     C     FUNKEY        IFEQ      '10'
142006070119     C                   END
142007070119     C     FUNKEY        IFEQ      '11'
142009070119     C                   END
142100970526     C     FUNKEY        IFEQ      '12'
142200970526     C                   EXSR      F12GES
142300970526     C                   END
142400970528     C     FUNKEY        IFEQ      '21'
142600970528     C                   END
142700970528     C     FUNKEY        IFEQ      '22'
142900970528     C                   END
143000970526     C     FUNKEY        IFEQ      '23'
143100970526     C                   EXSR      F23GES
143200970526     C                   END
143300970526     C     FUNKEY        IFEQ      '24'
143400970526     C                   EXSR      F24GES
143500970526     C                   END
143600970526     C     FUNKEY        IFEQ      'RU'
143700970526     C                   EXSR      RUPGES
143800970526     C                   END
143900970526     C*
144000970526     C                   END
144100970526     C*
144200970526     C                   ENDSR
144300970526     C*------------------------------------------------------------------------*
144400970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
144500970526     C*------------------------------------------------------------------------*
144600970526     C     FUNVAR        BEGSR
144700970526     C*
144800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
144900970526     C     KEY           IFEQ      ENTER
145000970526     C                   MOVEL     *BLANKS       FUNKEY
145100970526     C                   END
145200970526     C     KEY           IFEQ      F1
145300970526     C                   MOVEL     '01'          FUNKEY
145400970526     C                   END
145500970526     C     KEY           IFEQ      F2
145600970526     C                   MOVEL     '02'          FUNKEY
145700970526     C                   END
145800970526     C     KEY           IFEQ      F3
145900970526     C                   MOVEL     '03'          FUNKEY
146000970526     C                   END
146100970526     C     KEY           IFEQ      F4
146200970526     C                   MOVEL     '04'          FUNKEY
146300970526     C                   END
146400970526     C     KEY           IFEQ      F5
146500970526     C                   MOVEL     '05'          FUNKEY
146600970526     C                   END
146700970526     C     KEY           IFEQ      F6
146800970526     C                   MOVEL     '06'          FUNKEY
146900970526     C                   END
147000970526     C     KEY           IFEQ      F7
147100970526     C                   MOVEL     '07'          FUNKEY
147200970526     C                   END
147300970526     C     KEY           IFEQ      F8
147400970526     C                   MOVEL     '08'          FUNKEY
147500970526     C                   END
147600970526     C     KEY           IFEQ      F9
147700970526     C                   MOVEL     '09'          FUNKEY
147800970526     C                   END
147900970526     C     KEY           IFEQ      F10
148000970526     C                   MOVEL     '10'          FUNKEY
148100970526     C                   END
148200970526     C     KEY           IFEQ      F11
148300970526     C                   MOVEL     '11'          FUNKEY
148400970526     C                   END
148500970526     C     KEY           IFEQ      F12
148600970526     C                   MOVEL     '12'          FUNKEY
148700970526     C                   END
148800970526     C     KEY           IFEQ      F13
148900970526     C                   MOVEL     '13'          FUNKEY
149000970526     C                   END
149100970526     C     KEY           IFEQ      F14
149200970526     C                   MOVEL     '14'          FUNKEY
149300970526     C                   END
149400970526     C     KEY           IFEQ      F15
149500970526     C                   MOVEL     '15'          FUNKEY
149600970526     C                   END
149700970526     C     KEY           IFEQ      F16
149800970526     C                   MOVEL     '16'          FUNKEY
149900970526     C                   END
150000970526     C     KEY           IFEQ      F17
150100970526     C                   MOVEL     '17'          FUNKEY
150200970526     C                   END
150300970526     C     KEY           IFEQ      F18
150400970526     C                   MOVEL     '18'          FUNKEY
150500970526     C                   END
150600970526     C     KEY           IFEQ      F19
150700970526     C                   MOVEL     '19'          FUNKEY
150800970526     C                   END
150900970526     C     KEY           IFEQ      F20
151000970526     C                   MOVEL     '20'          FUNKEY
151100970526     C                   END
151200970526     C     KEY           IFEQ      F21
151300970526     C                   MOVEL     '21'          FUNKEY
151400970526     C                   END
151500970526     C     KEY           IFEQ      F22
151600970526     C                   MOVEL     '22'          FUNKEY
151700970526     C                   END
151800970526     C     KEY           IFEQ      F23
151900970526     C                   MOVEL     '23'          FUNKEY
152000970526     C                   END
152100970526     C     KEY           IFEQ      F24
152200970526     C                   MOVEL     '24'          FUNKEY
152300970526     C                   END
152400970526     C     KEY           IFEQ      ROLLUP
152500970526     C                   MOVEL     'RU'          FUNKEY
152600970526     C                   END
152700970526     C     KEY           IFEQ      HELP
152800970526     C                   MOVEL     'HP'          FUNKEY
152900970526     C                   END
153000970526     C*
153100970526     C                   ENDSR
153200970526     C*------------------------------------------------------------------------*
153300970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
153400970526     C*------------------------------------------------------------------------*
153500970526     C     FUNGES        BEGSR
153600970526     C*
153700970526     C* PULISCO CAMPI DI LAVORO
153800970526     C                   Z-ADD     0             W                 5 0
153900970526     C*
154000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
154100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
154200970526     C     1             DO        24            I
154300970526     C                   MOVEL     FUD(I)        FUDRIG
154400970526     C*
154500970526     C* PULIZIA FUNZIONI PRECEDENTI
154600970526     C                   MOVEL     *BLANKS       FUV(I)
154700970526     C*
154800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
154900970526     C* (SALTA F23 E F24)
155000970526     C     FUDATT        IFEQ      'S'
155100970526     C     FUDFUN        ANDNE     '23'
155200970526     C     FUDFUN        ANDNE     '24'
155300970526     C                   ADD       1             W
155400970526     C                   MOVEL     FUDDES        FUV(W)
155500970526     C                   END
155600970526     C                   END                                                    DO WHILE
155700970526     C*
155800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
155900970526     C* ALTRIMENTI DISATTIVO F24
156000970526     C     1             DO        24            I
156100970526     C                   MOVEL     FUD(I)        FUDRIG
156200970526     C*
156300970526     C     FUDFUN        IFEQ      '24'
156400970526     C     W             IFGT      6
156500970526     C                   MOVEL     'S'           FUDATT
156600970526     C                   ELSE
156700970526     C                   MOVEL     *BLANKS       FUDATT
156800970526     C                   END                                                    W GT 8
156900970526     C                   MOVEL     FUDRIG        FUD(I)
157000970526     C                   END
157100970526     C                   END                                                    DO WHILE
157200970526     C*
157300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
157400970526     C     W             DIV       6             I
157500970526     C                   MVR                     Z
157600970526     C*
157700970526     C     Z             IFNE      0
157800970526     C     I             ADD       1             Z
157900970526     C                   ELSE
158000970526     C                   Z-ADD     I             Z
158100970526     C                   END
158200970526     C*
158300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
158400970526     C                   ADD       1             FUNGRU
158500970526     C     FUNGRU        IFGT      Z
158600970526     C     FUNGRU        ORLT      1
158700970526     C                   Z-ADD     1             FUNGRU
158800970526     C                   END
158900970526     C*
159000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
159100970526     C     FUNGRU        IFEQ      1
159200970526     C                   MOVEL     FUV(1)        FUN001
159300970526     C                   MOVEL     FUV(2)        FUN002
159400970526     C                   MOVEL     FUV(3)        FUN003
159500970526     C                   MOVEL     FUV(4)        FUN004
159600970526     C                   MOVEL     FUV(5)        FUN005
159700970526     C                   MOVEL     FUV(6)        FUN006
159800970526     C                   END
159900970526     C*
160000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
160100970526     C     FUNGRU        IFEQ      2
160200970526     C                   MOVEL     FUV(7)        FUN001
160300970526     C                   MOVEL     FUV(8)        FUN002
160400970526     C                   MOVEL     FUV(9)        FUN003
160500970526     C                   MOVEL     FUV(10)       FUN004
160600970526     C                   MOVEL     FUV(11)       FUN005
160700970526     C                   MOVEL     FUV(12)       FUN006
160800970526     C                   END
160900970526     C*
161000970526     C* TERZO GRUPPO DI 6 FUNZIONI
161100970526     C     FUNGRU        IFEQ      3
161200970526     C                   MOVEL     FUV(13)       FUN001
161300970526     C                   MOVEL     FUV(14)       FUN002
161400970526     C                   MOVEL     FUV(15)       FUN003
161500970526     C                   MOVEL     FUV(16)       FUN004
161600970526     C                   MOVEL     FUV(17)       FUN005
161700970526     C                   MOVEL     FUV(18)       FUN006
161800970526     C                   END
161900970526     C*
162000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
162100970526     C     FUNGRU        IFEQ      4
162200970526     C                   MOVEL     FUV(19)       FUN001
162300970526     C                   MOVEL     FUV(20)       FUN002
162400970526     C                   MOVEL     FUV(21)       FUN003
162500970526     C                   MOVEL     FUV(22)       FUN004
162600970526     C                   MOVEL     *BLANKS       FUN005
162700970526     C                   MOVEL     *BLANKS       FUN006
162800970526     C                   END
162900970526     C*
163000970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
163100970526     C                   MOVEL     *BLANKS       FUN007
163200970526     C                   MOVEL     *BLANKS       FUN008
163300970526     C*
163400970526     C     1             DO        24            I
163500970526     C                   MOVEL     FUD(I)        FUDRIG
163600970526     C     FUDFUN        IFEQ      '23'
163700970526     C     FUDATT        ANDEQ     'S'
163800970526     C                   MOVEL     FUDDES        FUN007
163900970526     C                   END
164000970526     C     FUDFUN        IFEQ      '24'
164100970526     C     FUDATT        ANDEQ     'S'
164200970526     C                   MOVEL     FUDDES        FUN008
164300970526     C                   END
164400970526     C                   END                                                    DO
164500970526     C*
164600970526     C                   ENDSR
164700970526     C*------------------------------------------------------------------------*
164800970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
164900970526     C*------------------------------------------------------------------------*
165000970526     C     OPZGES        BEGSR
165100970526     C*
165200970526     C* PULISCO CAMPI DI LAVORO
165300970526     C                   Z-ADD     0             W                 5 0
165400970526     C*
165500970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
165600970526     C* E CARICO TABELLA OPZIONI ATTIVE
165700970526     C     1             DO        24            I
165800970526     C                   MOVEL     OPD(I)        OPDRIG
165900970526     C*
166000970526     C* PULIZIA OPZIONI PRECEDENTI
166100970526     C                   MOVEL     *BLANKS       OPV(I)
166200970526     C*
166300970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
166400970526     C     OPDATT        IFEQ      'S'
166500970526     C                   ADD       1             W
166600970526     C                   MOVEL     OPDDES        OPV(W)
166700970526     C                   END
166800970526     C                   END                                                    DO WHILE
166900970526     C*
167000970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
167100970526     C* ALTRIMENTI DISATTIVO F23
167200970526     C     1             DO        24            I
167300970526     C                   MOVEL     FUD(I)        FUDRIG
167400970526     C*
167500970526     C     FUDFUN        IFEQ      '23'
167600970526     C     W             IFGT      8
167700970526     C                   MOVEL     'S'           FUDATT
167800970526     C                   ELSE
167900970526     C                   MOVEL     *BLANKS       FUDATT
168000970526     C                   END                                                    W GT 8
168100970526     C                   MOVEL     FUDRIG        FUD(I)
168200970526     C                   END
168300970526     C                   END                                                    DO WHILE
168400970526     C*
168500970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
168600970526     C     W             DIV       8             I
168700970526     C                   MVR                     Z                 5 0
168800970526     C*
168900970526     C     Z             IFNE      0
169000970526     C     I             ADD       1             Z
169100970526     C                   ELSE
169200970526     C                   Z-ADD     I             Z
169300970526     C                   END
169400970526     C*
169500970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
169600970526     C                   ADD       1             OPZGRU
169700970526     C     OPZGRU        IFGT      Z
169800970526     C     OPZGRU        ORLT      1
169900970526     C                   Z-ADD     1             OPZGRU
170000970526     C                   END
170100970526     C*
170200970526     C* PRIMO GRUPPO DI 8 OPZIONI
170300970526     C     OPZGRU        IFEQ      1
170400970526     C                   MOVEL     OPV(1)        OPZ001
170500970526     C                   MOVEL     OPV(2)        OPZ002
170600970526     C                   MOVEL     OPV(3)        OPZ003
170700970526     C                   MOVEL     OPV(4)        OPZ004
170800970526     C                   MOVEL     OPV(5)        OPZ005
170900970526     C                   MOVEL     OPV(6)        OPZ006
171000970526     C                   MOVEL     OPV(7)        OPZ007
171100970526     C                   MOVEL     OPV(8)        OPZ008
171200970526     C                   END
171300970526     C*
171400970526     C* SECONDO GRUPPO DI 8 OPZIONI
171500970526     C     OPZGRU        IFEQ      2
171600970526     C                   MOVEL     OPV(9)        OPZ001
171700970526     C                   MOVEL     OPV(10)       OPZ002
171800970526     C                   MOVEL     OPV(11)       OPZ003
171900970526     C                   MOVEL     OPV(12)       OPZ004
172000970526     C                   MOVEL     OPV(13)       OPZ005
172100970526     C                   MOVEL     OPV(14)       OPZ006
172200970526     C                   MOVEL     OPV(15)       OPZ007
172300970526     C                   MOVEL     OPV(16)       OPZ008
172400970526     C                   END
172500970526     C*
172600970526     C* TERZO GRUPPO DI 8 OPZIONI
172700970526     C     OPZGRU        IFEQ      3
172800970526     C                   MOVEL     OPV(17)       OPZ001
172900970526     C                   MOVEL     OPV(18)       OPZ002
173000970526     C                   MOVEL     OPV(19)       OPZ003
173100970526     C                   MOVEL     OPV(20)       OPZ004
173200970526     C                   MOVEL     OPV(21)       OPZ005
173300970526     C                   MOVEL     OPV(22)       OPZ006
173400970526     C                   MOVEL     OPV(23)       OPZ007
173500970526     C                   MOVEL     OPV(24)       OPZ008
173600970526     C                   END
173700970526     C*
173800970526     C                   ENDSR
173900970526     C*------------------------------------------------------------------------*
174000970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
174100970526     C*------------------------------------------------------------------------*
174200970526     C     OPZCON        BEGSR
174300970526     C*
174400970526     C* PULISCO CAMPI DI LAVORO
174500970526     C                   Z-ADD     0             I                 5 0
174600970526     C                   Z-ADD     0             W                 5 0
174700970526     C*
174800970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
174900970526     C     1             DO        24            I
175000970526     C                   MOVEL     OPD(I)        OPDRIG
175100970526     C*
175200970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
175300970526     C     OPZKEY        IFEQ      OPDOPZ
175400970526     C     OPZKEY        OREQ      OPDOP1
175500970526     C*
175600970526     C     OPDATT        IFEQ      'S'
175700970526     C                   MOVEL     OPDOPZ        OPZKEY
175800970526     C                   Z-ADD     I             W
175900970526     C                   END
176000970526     C                   END
176100970526     C*
176200970526     C                   END                                                    DO WHILE
176300970526     C*
176400970526     C* OPZIONE NON ATTIVA (ERRORE)
176500970526     C     W             IFEQ      0
176600970526     C*
176700970526     C* OPZIONE NON OK
176800970526     C                   Z-ADD     1             OPZOK
176900970526     C*
177000970526     C* EMISSIONE MESSAGGIO DI ERRORE
177100970526     C                   MOVEL     MSG(1)        DSMSMS
177200970526     C                   ELSE
177300970526     C*
177400970526     C* OPZIONE ATTIVA (PROCEDI ...)
177500970526     C*
177600970526     C     OPZKEY        IFEQ      ' 1'
177700970526     C                   EXSR      O01GES
177800970526     C                   END
177900970526     C     OPZKEY        IFEQ      ' 2'
178000970526     C                   EXSR      O02GES
178100970526     C                   END
178200991014     C     OPZKEY        IFEQ      ' 3'
178300991014     C                   EXSR      O03GES
178400991014     C                   END
178500070119     C     OPZKEY        IFEQ      ' 4'
178501070403     C***                EXSR      O04GES
178700070119     C                   END
178800970526     C     OPZKEY        IFEQ      ' 5'
178900970526     C                   EXSR      O05GES
179000970526     C                   END
179100970526     C     OPZKEY        IFEQ      ' 7'
179101070403     C***                EXSR      O07GES
179300970526     C                   END
179301070411     C     OPZKEY        IFEQ      '10'
179302070411     C                   EXSR      O10GES
179303070411     C                   END
180300970526     C                   END                                                    OPZ.NON ATTIVA
180400970526     C*
180500970526     C                   ENDSR
180600970526     C*------------------------------------------------------------------------*
180700970526     C* NOTE COSTRUZIONE SCHIERE
180800970526     C*------------------------------------------------------------------------*
180900970526     C*
181000970526     C* -MSG-MESSAGGI
181100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
181200970526     C* (NON TOCCARE MSG,1 E MSG,2)
181300970526     C*
181400970526     C* -CMD-COMANDI
181500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
181600970526     C*
181700970526     C* -OPD-OPZIONI
181800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
181900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
182000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
182100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
182200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
182300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
182400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
182500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
182600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
182700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
182800970526     C*
182900970526     C* -FUD-FUNZIONI
183000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
183100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
183200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
183300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
183400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
183500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
183600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
183700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
183800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
183900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
184000970526     O*------------------------------------------------------------------------*
184100970526** DEC - DECODIFICHE
184101070119     SCELTA                                                      1
184102070122 INTERROGAZIONE                                                  2
184200070122    GESTIONE                                                     3
184201070122  GESTIONE FULL                                                  4
184400070122                                                                 5
184700970526** MSG - MESSAGGI
184800070403FIDG11R- Opzione non gestita                                                   1
184900070403FIDG11R- Tasto funzionale non gestito                                          2
185000070403FIDG11R- 3                                                                     3
185100070403FIDG11R- Record annullato. Consentito solo il ripristino                       4
185200070403FIDG11R- Record non annullato. Impossibile ripristinare                        5
185300070403FIDG11R- Nessun record corrispondente ai requisiti richiesti                   6
185301070403FIDG11R- Trovati troppi record corrispondenti ai requisiti richiesti           7
185500070403FIDG11R- Utente non abilitato                                                  8
185600070403FIDG11R- 9                                                                     9
185700070403FIDG11R- 10                                                                   10
185800070403FIDG11R- 11                                                                   11
185900070403FIDG11R- Data errata                                                          12
186000070403FIDG11R- 13                                                                   13
186100070403FIDG11R- 14                                                                   14
186200070403FIDG11R- 15                                                                   15
186300070403FIDG11R- 16                                                                   16
186400070403FIDG11R- 17                                                                   17
186500070403FIDG11R- 18                                                                   18
186600070403FIDG11R- 19                                                                   19
186700070403FIDG11R- 20                                                                   20
186800970528** CMD - COMANDI CL
186900970528WRKSPLF
187000970528
187100970528
187200970528
187300970528
187400970526** OPD - OPZIONI
187500970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
187600970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
187700991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
187800070403 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
187900970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
188000000703                   -6 -     (S/N)=N                        opz. 06¬
188100970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
188200970527                   -8 -     (S/N)=N                        opz. 08¬
188300070119                   -9 -     (S/N)=N                        opz. 09¬
18840007041110=Anagr. giri     -10-     (S/N)=N                        opz. 10¬
188500070119                   -11-     (S/N)=N                        opz. 11¬
188600970526                   -  -     (S/N)=N                        opz. 12¬
188700970526                   -  -     (S/N)=N                        opz. 13¬
188800970526                   -  -     (S/N)=N                        opz. 14¬
188900970526                   -  -     (S/N)=N                        opz. 15¬
189000970526                   -  -     (S/N)=N                        opz. 16¬
189100970526                   -  -     (S/N)=N                        opz. 17¬
189200970526                   -  -     (S/N)=N                        opz. 18¬
189300970526                   -  -     (S/N)=N                        opz. 19¬
189400970526                   -  -     (S/N)=N                        opz. 20¬
189500970526                   -  -     (S/N)=N                        opz. 21¬
189600970526                   -  -     (S/N)=N                        opz. 22¬
189700970526                   -  -     (S/N)=N                        opz. 23¬
189800970526                   -  -     (S/N)=N                        opz. 24¬
189900970526** FUD - FUNZIONI
190000970526                   -        (S/N)=N                        funz.01¬
190100970526                   -        (S/N)=N                        funz.02¬
190200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
190300970526                   -        (S/N)=N                        funz.04¬
190400970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
190500970526F06=Inserimento    -        (S/N)=N                        funz.06¬
190600970526                   -        (S/N)=N                        funz.07¬
190700070403                   -        (S/N)=N                        funz.08¬
190800970527                   -        (S/N)=N                        funz.09¬
190900070403                   -        (S/N)=N                        funz.10¬
191000070403                   -        (S/N)=N                        funz.11¬
191100970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
191200970526                   -        (S/N)=N                        funz.13¬
191300970526                   -        (S/N)=N                        funz.14¬
191400970526                   -        (S/N)=N                        funz.15¬
191500970526                   -        (S/N)=N                        funz.16¬
191600970526                   -        (S/N)=N                        funz.17¬
191700970526                   -        (S/N)=N                        funz.18¬
191800970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
191900970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
192000070205F21=Stampa elenco  -        (S/N)=N                        funz.21¬
192100070403F22=Gestione stampe-        (S/N)=N                        funz.22¬
192200070403F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
192300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
