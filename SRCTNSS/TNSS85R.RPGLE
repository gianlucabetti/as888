000100000000     H DECEDIT('0,') DATEDIT(*YMD.)
000200000000      **************************************************************************
000300000000      *
000400000000      *  Nome programma:  TNSS85R
000500000000      *  Descrizione   :  Stampa statistica transiti.
000600000000      *                   Programma di esecuzione, batch.
000700000000      *  Autore        :  STEFANO (SC)
000800000000      *  Data creazione:  18 APR 1994
000900000000      *
001000000000      **************************************************************************
001100000000     FAZORG01L  IF   E           K DISK
001200990609     FSISDT03L  IF   E           K DISK    USROPN
001300000000     FWFSDT01L  UF A E           K DISK
001400000000     FTNSS85P   O    E             PRINTER OFLIND(*IN01)
001500000000     F                                     USROPN
001600000000      *
001700000000      * DS x il passaggio parametri
001800000000     D PARAM           DS
001900990608     D  ANNODA                 1      4  0
002000990608     D  MESEDA                 5      6  0
002100990608     D  DATADA                 1      6  0
002200990608     D  ANNOA                  7     10  0
002300990608     D  MESEA                 11     12  0
002400990608     D  DATAA                  7     12  0
002500000000     D KPJBA         E DS
002600040618     d sttc            s              3  0 dim(5) inz
002700040618     d stt             s              3  0 dim(5) inz descend
002800040618     d impc            s             15  3 dim(5) inz
002900040618     d imp             s             15  3 dim(5) inz
003000000000      *
003100990609     C     SISDTK        KLIST                                                  SISDT03L
003200000000     C                   KFLD                    ANNODA
003300000000     C                   KFLD                    MESEDA
003400000000     C     WFSDTK        KLIST                                                  WFSDT01L
003500000000     C                   KFLD                    WFTTTR
003600000000     C                   KFLD                    WFTTE1
003700040618     C                   KFLD                    WFTTT1
003800040618     C                   KFLD                    WFTTT2
003900040618     C                   KFLD                    WFTTT3
004000040618     C                   KFLD                    WFTTT4
004100000000     C                   KFLD                    WFTTE2
004200000000      *
004300000000     C     *ENTRY        PLIST
004400000000     C                   PARM                    KPJBA
004500000000     C                   MOVEL     KPJBU         PARAM
004600000000      *
004700000000      **************************************************************************
004800000000      *                    M A I N    L I N E
004900000000      **************************************************************************
005000020906     C                   Z-ADD     1             CODUT             1 0
005100000000      *
005200000000      *?  FASE 1 :   LETTURA SALDI E SCRITTURA FILE DI LAVORO  ?
005300990609     C                   OPEN      SISDT03L
005400000000      *
005500000000      * Posiziono il file e inizio le letture
005600990609     C     SISDTK        SETLL     SISDT03L
005700000000     C                   DO        *HIVAL
005800990609     C                   READ      SISDT03L                               02
005900000000     C  N02SDTANN        IFEQ      ANNOA                                        ANNO FINALE
006000000000     C     SDTMES        ANDGT     MESEA                                        MESE FINALE
006100000000     C                   SETON                                        02        FINE LETTURE
006200000000     C                   ENDIF
006300040618     c   02              leave
006400040618     c* controllo record
006500040618     C     SDTNSP        IFEQ      *ZEROS                                       --- 1 -->
006600040618     C     SDTPKG        ANDEQ     *ZEROS
006700040618     C     SDTNCO        ANDEQ     *ZEROS
006800040618     C     SDTVOL        ANDEQ     *ZEROS
006900040618     C     SDTCT1        ANDEQ     *ZEROS
007000040618     c                   iter
007100040618     c                   end
007200040618      * carico la schiera dei 5 transiti e la ordino
007300040618     c                   exsr      srstt
007400040618      * Considero solo se almeno un transito e' pieno
007500040618     c                   if        stt(1) = 0
007600040618     c                   iter
007700040618     c                   end
007800040618     c* memorizzo il record nel work file
007900000000     C                   EXSR      ELABOR                                       ELABORAZIONE
008000040618     C                   ENDDO                                                  *HIVAL
008100000000      *
008200990609     C                   CLOSE     SISDT03L
008300000000      *
008400000000      *?  FASE 2 :   LETTURA FILE DI LAVORO E STAMPA TABULATO  ?
008500000000      *
008600000000     C                   OPEN      TNSS85P                                      PRINTER FILE
008700000000      *
008800000000     C                   WRITE     SS85T1                                       INTESTAZIONE
008900000000     C                   SETOFF                                       01
009000000000     C                   Z-ADD     *ZERO         $ALMEN            1 0          ALMENO UNO
009100000000     C                   Z-ADD     *ZERO         $RIGHE            5 0
009200000000     C                   Z-ADD     *ZERO         TERMIP
009300040618     C                   Z-ADD     *ZERO         TERMP
009400040618     C                   Z-ADD     *ZERO         TERMa
009500000000      *
009600000000      * Leggo dall'archivio di lavoro
009700000000     C     *LOVAL        SETLL     WFSDT01L
009800000000     C                   DO        *HIVAL
009900000000     C                   READ(N)   WFSDT01L                               02
010000000000     C  N02              EXSR      DETTAG                                       STAMPA DETTAGL.
010100000000     C  N02              ENDDO                                                  *HIVAL
010200000000      *
010300000000      * Termino verificando se ho stampato qualcosa
010400000000     C     $ALMEN        IFNE      *ZERO
010500000000     C                   EXSR      TOTALI                                       STAMPA TOTALI
010600000000     C                   WRITE     SS85E1                                       FINE STAMPA
010700000000     C                   ELSE
010800000000     C                   WRITE     SS85E2                                       VUOTA
010900000000     C                   ENDIF
011000000000     C                   CLOSE     TNSS85P
011100000000      *
011200000000     C                   SETON                                        LR
011300040618      **************************************************************************
011400040618      * Elaborazione record letti da SISDT
011500040618      **************************************************************************
011600040618     C     ELABOR        BEGSR
011700040618     c                   do        5             x
011800040618     c                   if        stt(x) = 0
011900040618     c                   leave
012000040618     c                   end
012100990608     C                   CLEAR                   WFSDT000
012200040618     C                   Z-ADD     SDTTFP        WFTTE1                         T.PARTENZA
012300040618     C                   Z-ADD     SDTTFA        WFTTE2                         T.ARRIVO
012400040618     c                   select
012500040618     c                   when      x=1
012600040618     C                   Z-ADD     StT(1)        WFTTTR                         T.TRANSITO
012700040618     C                   Z-ADD     stt(2)        WFTTT1                         ALTRO TRANSITO
012800040618     C                   Z-ADD     stt(3)        WFTTT2                         ALTRO TRANSITO
012900040618     C                   Z-ADD     stt(4)        WFTTT3                         ALTRO TRANSITO
013000040618     C                   Z-ADD     stt(5)        WFTTT4                         ALTRO TRANSITO
013100040618     c                   when      x=2
013200040618     C                   Z-ADD     StT(2)        WFTTTR                         T.TRANSITO
013300040618     C                   Z-ADD     stt(1)        WFTTT1                         ALTRO TRANSITO
013400040618     C                   Z-ADD     stt(3)        WFTTT2                         ALTRO TRANSITO
013500040618     C                   Z-ADD     stt(4)        WFTTT3                         ALTRO TRANSITO
013600040618     C                   Z-ADD     stt(5)        WFTTT4                         ALTRO TRANSITO
013700040618     c                   when      x=3
013800040618     C                   Z-ADD     StT(3)        WFTTTR                         T.TRANSITO
013900040618     C                   Z-ADD     stt(1)        WFTTT1                         ALTRO TRANSITO
014000040618     C                   Z-ADD     stt(2)        WFTTT2                         ALTRO TRANSITO
014100040618     C                   Z-ADD     stt(4)        WFTTT3                         ALTRO TRANSITO
014200040618     C                   Z-ADD     stt(5)        WFTTT4                         ALTRO TRANSITO
014300040618     c                   when      x=4
014400040618     C                   Z-ADD     StT(4)        WFTTTR                         T.TRANSITO
014500040618     C                   Z-ADD     stt(1)        WFTTT1                         ALTRO TRANSITO
014600040618     C                   Z-ADD     stt(2)        WFTTT2                         ALTRO TRANSITO
014700040618     C                   Z-ADD     stt(3)        WFTTT3                         ALTRO TRANSITO
014800040618     C                   Z-ADD     stt(5)        WFTTT4                         ALTRO TRANSITO
014900040618     c                   when      x=5
015000040618     C                   Z-ADD     StT(5)        WFTTTR                         T.TRANSITO
015100040618     C                   Z-ADD     stt(1)        WFTTT1                         ALTRO TRANSITO
015200040618     C                   Z-ADD     stt(2)        WFTTT2                         ALTRO TRANSITO
015300040618     C                   Z-ADD     stt(3)        WFTTT3                         ALTRO TRANSITO
015400040618     C                   Z-ADD     stt(4)        WFTTT4                         ALTRO TRANSITO
015500040618     c                   endsl
015600000000     C     WFSDTK        CHAIN     WFSDT01L                           03
015700040618      * competenze
015800040618     c                   select
015900040618     c                   when      x=1
016000040618     C                   add       imp(1)        WFTCTL                         COMPETENZE
016100040618     c                   when      x=2
016200040618     C                   ADD       imp(2)        WFTCTL                         COMPETENZE
016300040618     c                   when      x=3
016400040618     C                   ADD       imp(3)        WFTCTL                         COMPETENZE
016500040618     c                   when      x=4
016600040618     C                   ADD       imp(4)        WFTCTL                         COMPETENZE
016700040618     c                   when      x=5
016800040618     C                   ADD       imp(5)        WFTCTL                         COMPETENZE
016900040618     c                   endsl
017000000000     C                   ADD       SDTNSP        WFTNSP                         NR.SPEDIZIONI
017100000000     C                   ADD       SDTPKG        WFTPKG                         PESO (KG)
017200000000     C                   ADD       SDTNCO        WFTNCO                         NR.COLLI
017300000000     C                   ADD       SDTVOL        WFTVOL                         VOLUME (MC)
017400990608     C   03              WRITE     WFSDT000
017500990608     C  N03              UPDATE    WFSDT000
017600040618     c                   enddo
017700000000      *
017800000000     C                   ENDSR
017900040618      **************************************************************************
018000040618      * carica i 5 transiti in schiera
018100040618      **************************************************************************
018200040618     C     srstt         BEGSR
018300040618     c                   clear                   stt
018400040618     c                   clear                   sttc
018500040618     c                   clear                   impc
018600040618     c                   clear                   imp
018700040618     c                   clear                   x                 2 0
018800040618     c                   if        sdttt1 <> 0
018900040618     c                   add       1             x
019000040618     c                   z-add     sdttt1        sttc(x)
019100040618     c                   z-add     sdtct1        impc(x)
019200040618     c                   end
019300040618     c                   if        sdttt2 <> 0
019400040618     c                   add       1             x
019500040618     c                   z-add     sdttt2        sttc(x)
019600040618     c                   z-add     sdtct2        impc(x)
019700040618     c                   end
019800040618     c                   if        sdttt3 <> 0
019900040618     c                   add       1             x
020000040618     c                   z-add     sdttt3        sttc(x)
020100040618     c                   z-add     sdtct3        impc(x)
020200040618     c                   end
020300040618     c                   if        sdttt4 <> 0
020400040618     c                   add       1             x
020500040618     c                   z-add     sdttt4        sttc(x)
020600040618     c                   z-add     sdtct4        impc(x)
020700040618     c                   end
020800040618     c                   if        sdttt5 <> 0
020900040618     c                   add       1             x
021000040618     c                   z-add     sdttt5        sttc(x)
021100040618     c                   z-add     sdtct5        impc(x)
021200040618     c                   end
021300040618     c                   movea     sttc          stt
021400040618     c                   sorta     stt
021500040618     c* carico schiera importi
021600040618     c                   do        5             x
021700040618     c                   if        stt(x) = 0
021800040618     c                   leave
021900040618     c                   end
022000040618     c                   z-add     1             y                 2 0
022100040618     c     stt(x)        lookup    sttc(y)                                55
022200040618     c                   if        *in55
022300040618     c                   z-add     impc(y)       imp(x)
022400040618     c                   end
022500040618     c                   enddo
022600040618     c                   endsr
022700000000      **************************************************************************
022800000000      * Stampa del dettaglio per terminal
022900000000      **************************************************************************
023000000000     C     DETTAG        BEGSR
023100000000      * Muovo i campi dal file alla stampa
023200040618     C                   Z-ADD     WFTTE1        TERMP                          1° COLONNA
023300040618     C                   Z-ADD     WFTTT1        TERM2P                         2° COLONNA
023400040618     C                   Z-ADD     WFTTT2        TERM3P                         3° COLONNA
023500040618     C                   Z-ADD     WFTTT3        TERM4P                         3° COLONNA
023600040618     C                   Z-ADD     WFTTT4        TERM5P                         3° COLONNA
023700040618     C                   Z-ADD     WFTTE2        TERMA                          4° COLONNA
023800000000      *
023900000000     C     WFTPKG        DIV       100           PKGP                           PESO
024000000000     C                   Z-ADD     WFTVOL        VOLP                           VOLUME
024100000000     C                   Z-ADD     WFTNCO        NCOP                           NR.COLLI
024200000000     C                   Z-ADD     WFTNSP        NSPP                           NR.SPEDIZIONI
024300040618     C                   Z-ADD     WFTCTL        COMP                           COMPETENZE
024400000000      *-------------------------------------------------------------------------
024500000000      * Testo la chiave principale del file, la filiale di transito che devo
024600000000      * emettere in testa alla pagina
024700000000      * Se diversa dalla precedente, stampo la testata
024800000000     C     WFTTTR        IFNE      TERMIP
024900000000     C                   Z-ADD     WFTTTR        TERMIP
025000000000     C     TERMIP        CHAIN     AZORG01L                           99
025100000000     C  N99              MOVEL     ORGDES        DETERP                         DESCRIZIONE
025200000000     C   99              MOVE      *BLANKS       DETERP                         DESCRIZIONE
025300000000      *
025400000000     C     $ALMEN        IFNE      *ZERO                                        NO PRIMA VOLTA
025500000000     C                   EXSR      TOTALI                                       TOTALI TERMINAL
025600000000     C                   ENDIF
025700000000      *
025800000000     C                   WRITE     SS85T1                                       NUOVA PAGINA
025900000000     C                   SETOFF                                       01
026000000000     C                   WRITE     SS85T2                                       TESTATA
026100000000     C                   ENDIF
026200040618      * rottura terminal partenza
026300040618     C     TERMp         IFNE      $TE1PR                                       1°  C O L O N  N A
026400040618     C*                  EXSR      TOTTER                                       TOTALI TERM.
026500040618     C     TERMp         CHAIN     AZORG01L                           99
026600040618     C  N99              MOVEL     ORGDES        DETEP                          DESCRIZIONE
026700040618     C   99              MOVE      *BLANKS       DETEP                          DESCRIZIONE
026800040618     C                   Z-ADD     TERMp         $TE1PR            3 0
026900040618     C                   ENDIF
027000040618      * rottura terminal arrivo
027100040618     C     TERMa         IFNE      $TE2PR                                       1°  C O L O N  N A
027200040618     C*                  EXSR      TOTTER                                       TOTALI TERM.
027300040618     C     TERMa         CHAIN     AZORG01L                           99
027400040618     C  N99              MOVEL     ORGDES        DETEa                          DESCRIZIONE
027500040618     C   99              MOVE      *BLANKS       DETEa                          DESCRIZIONE
027600040618     C                   Z-ADD     TERMa         $TE2PR            3 0
027700040618     C                   ENDIF
027800000000      * Per overflow ristampo instestazione
027900000000     C   01              DO
028000000000     C                   WRITE     SS85T1                                       INTESTAZIONE
028100000000     C                   WRITE     SS85T2                                       TESTATA
028200000000     C                   SETOFF                                       01
028300000000     C                   ENDDO
028400000000      * Stampo la riga di dettaglio
028500000000     C                   WRITE     SS85D1                                       DETTAGLIO
028600000000     C                   Z-ADD     1             $ALMEN                         ALMENO UNO
028700000000     C                   ADD       1             $RIGHE                         CONTA RIGHE
028800000000      * Mantengo aggiornati i totali
028900000000     C                   ADD       PKGP          TTEPKG
029000000000     C                   ADD       VOLP          TTEVOL
029100000000     C                   ADD       NCOP          TTENCO
029200000000     C                   ADD       NSPP          TTENSP
029300040618     C                   ADD       COMP          TTECOM
029400000000      *
029500000000     C                   ADD       PKGP          TOTPKG
029600000000     C                   ADD       VOLP          TOTVOL
029700000000     C                   ADD       NCOP          TOTNCO
029800000000     C                   ADD       NSPP          TOTNSP
029900040618     C                   ADD       COMP          TOTCOM
030000000000      *
030100000000     C                   ENDSR
030200000000      **************************************************************************
030300000000      * Stampa dei totali
030400000000      **************************************************************************
030500000000     C     TOTALI        BEGSR
030600000000      *
030700000000      * Emetto prima i totali per terminal
030800040618     C*                  EXSR      TOTTER                                       TOTALI TERM.
030900000000      *
031000000000     C                   WRITE     SS85X1                                       STAMPA TOTALI
031100000000     C                   Z-ADD     *ZERO         $RIGHE
031200000000      *
031300000000     C                   Z-ADD     *ZERO         TOTPKG
031400000000     C                   Z-ADD     *ZERO         TOTVOL
031500000000     C                   Z-ADD     *ZERO         TOTNCO
031600000000     C                   Z-ADD     *ZERO         TOTNSP
031700000000     C                   Z-ADD     *ZERO         TOTCOM
031800000000      *
031900000000     C                   ENDSR
032000000000      **************************************************************************
032100000000      * Stampa dei totali per terminal di arrivo/partenza
032200000000      **************************************************************************
032300000000     C     TOTTER        BEGSR
032400000000      *
032500000000      * Stampo il totale solo se ho gia' stampato piu' di una riga di dettaglio
032600040618     C     $RIGHE        IFGT      1
032700000000     C                   WRITE     SS85X2                                       STAMPA TOTALI
032800000000     C                   ENDIF
032900000000     C                   Z-ADD     *ZERO         $RIGHE
033000000000      *
033100000000     C                   Z-ADD     *ZERO         TTEPKG
033200000000     C                   Z-ADD     *ZERO         TTEVOL
033300000000     C                   Z-ADD     *ZERO         TTENCO
033400000000     C                   Z-ADD     *ZERO         TTENSP
033500000000     C                   Z-ADD     *ZERO         TTECOM
033600000000      *
033700000000     C                   ENDSR
