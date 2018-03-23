000100000000     H*------------------------------------------------------------------------*
000200970623     H* Creazione sinonimi da località
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800970623     FAZCPC01L  IF   E           K DISK
000900970624     FAZCPL01L  UF   E           K DISK
001000970624     FAZCPS01L  IF A E           K DISK
001100000000     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001200000000     F                                     FORMLEN(66)
001300000000     F                                     FORMOFL(60)
001400000000     D*------------------------------------------------------------------------*
001500970626     D* Data Structure
001600000000     D*------------------------------------------------------------------------*
001700000000     D*-------------------
001800970623     D* SCHIERA SINONIMI
001900000000     D*-------------------
002000970626     D sin             S             35    DIM(60)
002100970626     D*-------------------
002200970626     D* INTESTAZIONI STAMPA
002300970626     D*-------------------
002400970626     D TES             S             66    DIM(10) CTDATA PERRCD(1)
002500000000     D*-------------------
002600000000     D* ARCHITETTURA
002700000000     D*-------------------
002800000000     D KPJBA         E DS
002900000000     D*-------------------
003000000000     D* DS PARAMETRI DI LANCIO PGM
003100000000     D*-------------------
003200970624     D TISI88DS      E DS
003300950703     D PARAM           DS
003400970623     D  PARTLA                        1
003500970623     D  PARCA1                        9
003600970623     D  PARCA2                        9
003700970623     D  PARNA1                        3
003800970623     D  PARNA2                        3
003900970623     D  PARPR1                        2
004000970623     D  PARPR2                        2
004100970623     D  PARLN1                        3  0
004200970623     D  PARLN2                        3  0
004300970623     D  PARSTA                        1
004400970623     D  PARVER                        5  0
004500970623     D  PARDRI                        8  0
004600970917     D  PARLSS                        1
004700000000     C*------------------------------------------------------------------------*
004800000000     C* MAIN LINE
004900000000     C*------------------------------------------------------------------------*
005000000000     C*
005100970625     C* stampa testata
005200970625     C                   EXCEPT    TESTA
005300970625     C                   SETOFF                                       OF
005400970624     c*
005500000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
005600970623     C                   EXSR      setcpc
005700000000     C*
005800970623     C* CICLO FINO A ROTTURA DI cap
005900970626DO  1C     fincpc        DOWEQ     '0'
006000000000     C*
006100970623     C* OPERAZIONI PER NUOVA località
006200970623     C                   EXSR      setcpl
006300000000     C*
006400970624     C* CICLO FINO A ROTTURA DI cap nel file località
006500970626DO  2C     fincpl        DOWEQ     '0'
006600000000     C*
006700970623     C* lancia pgm che da località trova i sinonimi
006800970623     C                   EXSR      lanpgm
006900970623     C*
007000970623     C* scrive le località trovate nel file sinonimi
007100970623     C                   EXSR      scrive
007200000000     C*
007300970623     C* LEGGE IL RECORD SUCCESSIVO del file località
007400970623     C                   EXSR      letcpl
007500000000     C*
007600970626E   2C                   ENDDO
007700970624     C*
007800970624     C* LEGGE IL RECORD SUCCESSIVO del file cap
007900970624     C                   EXSR      letcpc
008000970626E   1C                   ENDDO
008100000000     C*
008200000000     C* STAMPA RIGA FINE STAMPA
008300000000     C                   EXCEPT    STAFIN
008400970624     C*
008500970624     C* aggiorna flag esistono sinonimi sul file località
008600970626if  1c     partla        IFEQ      'A'
008700970625     C                   EXSR      aggloc
008800970626e   1c                   ENDIF
008900000000     C*
009000000000     C                   SETON                                        LR
009100000000     C*------------------------------------------------------------------------*
009200970623     C* setcpc - POSIZIONAMENTO E PRIMA LETTURA DEL FILE cappario
009300000000     C*------------------------------------------------------------------------*
009400970623     C     setcpc        BEGSR
009500000000     C*
009600000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
009700970624     C                   MOVEL     '0'           fincpc
009800970623     C                   Z-ADD     parver        cpcver
009900970623     C                   MOVEL     *blanks       cpcnar
010000970624     C                   MOVEL     *blanks       cpccap
010100970624     C     keycpc        SETLL     azcpc01L                           99        *FINE-ARCHIV
010200000000     C*
010300970626if  1C     *IN99         IFEQ      '1'
010400970624     C                   MOVEL     '1'           fincpc            1
010500000000     C                   ELSE
010600970623     C                   EXSR      letcpc
010700970626e   1C                   ENDif
010800000000     C*
010900000000     C                   ENDSR
011000000000     C*------------------------------------------------------------------------*
011100970623     C* letcpc - LETTURA PROSSIMO RECORD cappario
011200000000     C*------------------------------------------------------------------------*
011300970623     C     letcpc        BEGSR
011400000000     C*
011500000000     C* LEGGO FINO A:
011600000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
011700000000     C*     - TROVATO RECORD VALIDO
011800000000     C                   MOVEL     'N'           WRECOK
011900970626do  1C     fincpc        DOWEQ     '0'
012000000000     C     WRECOK        ANDEQ     'N'
012100000000     C*
012200000000     C* LETTURE SUCCESSIVE ALLA PRIMA
012300970623     C                   READ      azcpc01L                               99
012400000000     C*
012500970626if  2C     *IN99         IFEQ      '1'                                          *FINE FILE
012600970623     C                   MOVEL     '1'           fincpc
012700970626x   1C                   ELSE
012800970626     C                   EXSR      chkcpc                                       *controllo record
012900970626e   2C                   ENDif
013000970626e   1C                   ENDdo
013100000000     C*
013200000000     C                   ENDSR
013300000000     C*------------------------------------------------------------------------*
013400970623     C* chkcpc - CONTROLLA VALIDITA' DEL RECORD cappario
013500000000     C*------------------------------------------------------------------------*
013600970623     C     chkcpc        BEGSR
013700000000     C*
013800000000     C                   MOVEL     'S'           WRECOK            1
013900970624     C*
014000970624     C* annullato
014100970626if  1c     cpcatb        IFNE      *blanks
014200970624     C                   MOVEL     'N'           WRECOK
014300970626e   1c                   ENDIF
014400000726     C*
014500000726     C* obsoleto
014600000726if  1c     cpcfi1        IFEQ      'O '
014700000726     C                   MOVEL     'N'           WRECOK
014800000726e   1c                   ENDIF
014900000000     C*
015000970623     C* cap
015100970626if  1C     parca1        IFNE      *blanks
015200970626if  2C     cpccap        IFLT      parca1
015300000000     C                   MOVEL     'N'           WRECOK
015400970626e   2C                   ENDIF
015500970626e   1C                   ENDIF
015600970626if  1c     parca2        IFNE      *blanks
015700970626if  2C     cpccap        IFGT      parca2
015800970623     C                   MOVEL     'N'           WRECOK
015900970626e   2C                   ENDIF
016000970626e   1C                   ENDIF
016100970623     c*
016200970623     C* nazione
016300970626if  1C     parna1        IFNE      *blanks
016400970626if  2C     cpcnar        IFLT      parna1
016500970623     C                   MOVEL     'N'           WRECOK
016600970626e   2C                   ENDIF
016700970626e   1C                   ENDIF
016800970626if  1C     parna2        IFNE      *blanks
016900970626if  2C     cpcnar        IFGT      parna2
017000970623     C                   MOVEL     'N'           WRECOK
017100970626e   2C                   ENDIF
017200970626e   1C                   ENDIF
017300970623     c*
017400970623     C* provincia
017500970626if  1C     parpr1        IFNE      *blanks
017600970626if  2C     cpcprv        IFLT      parpr1
017700970623     C     cpcpra        ORNE      *blanks
017800970623     C     cpcpra        ANDLT     parpr1
017900970623     C                   MOVEL     'N'           WRECOK
018000970626e   2C                   ENDIF
018100970626e   1C                   ENDIF
018200970626if  1C     parpr2        IFNE      *blanks
018300970626if  2C     cpcprv        IFLT      parpr2
018400970623     C     cpcpra        ORNE      *blanks
018500970623     C     cpcpra        ANDLT     parpr2
018600970623     C                   MOVEL     'N'           WRECOK
018700970626e   2C                   ENDIF
018800970626e   1C                   ENDIF
018900970623     c*
019000970623     C* linea arrivo
019100970626if  1C     parln1        IFNE      *zeros
019200970626if  2C     cpclna        IFLT      parln1
019300970623     C                   MOVEL     'N'           WRECOK
019400970626e   2C                   ENDIF
019500970626e   1C                   ENDIF
019600970626if  1C     parln2        IFNE      *zeros
019700970626if  2C     cpclna        IFGT      parln2
019800970623     C                   MOVEL     'N'           WRECOK
019900970626e   2C                   ENDIF
020000970626e   1C                   ENDIF
020100000000     C*
020200000000     C                   ENDSR
020300970623     C*------------------------------------------------------------------------*
020400970623     C* setcpl - POSIZIONAMENTO E PRIMA LETTURA DEL FILE località
020500970623     C*------------------------------------------------------------------------*
020600970623     C     setcpl        BEGSR
020700970623     C*
020800970623     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
020900970624     C                   MOVEL     '0'           fincpl
021000970623     C                   Z-ADD     parver        cplver
021100970624     C                   MOVEL     cpcnar        cplnar
021200970624     C                   MOVEL     cpccap        cplcap
021300970624     C                   MOVEL     *blanks       cplloc
021400970624     C     keycpl        SETLL     azcpl01L                           99        *FINE-ARCHIV
021500970623     C*
021600970626if  1C     *IN99         IFEQ      '1'
021700970624     C                   MOVEL     '1'           fincpl            1
021800970626x   1C                   ELSE
021900970623     C                   EXSR      letcpl
022000970626e   1C                   ENDIF
022100970623     C*
022200970623     C                   ENDSR
022300970623     C*------------------------------------------------------------------------*
022400970623     C* letcpl - LETTURA PROSSIMO RECORD località
022500970623     C*------------------------------------------------------------------------*
022600970623     C     letcpl        BEGSR
022700970623     C*
022800970623     C* LEGGO FINO A:
022900970623     C*     - FINE FILE O SUPERATO LIMITE FINALE
023000970623     C*     - TROVATO RECORD VALIDO
023100970623     C                   MOVEL     'N'           WRECOK
023200970626do  1C     fincpl        DOWEQ     '0'
023300970623     C     WRECOK        ANDEQ     'N'
023400970623     C*
023500970623     C* LETTURE SUCCESSIVE ALLA PRIMA
023600970623     C                   READ      azcpl01L                               99
023700970626if  2C     *IN99         IFEQ      '1'                                          *FINE FILE
023800970624     C     cplver        ORNE      cpcver
023900970624     C     cplnar        ORNE      cpcnar
024000970624     C     cplcap        ORNE      cpccap
024100970623     C                   MOVEL     '1'           fincpl
024200970626x   2C                   ELSE
024300970623     C                   EXSR      chkcpl                                       *CONTR.VALIDITA REC.
024400970626e   2C                   ENDif
024500970626e   1C                   ENDDO
024600970623     C*
024700970623     C                   ENDSR
024800970623     C*------------------------------------------------------------------------*
024900970623     C* chkcpl - CONTROLLA VALIDITA' DEL RECORD località
025000970623     C*------------------------------------------------------------------------*
025100970623     C     chkcpl        BEGSR
025200970623     C*
025300970623     C                   MOVEL     'S'           WRECOK            1
025400970624     C*
025500970624     C* annullato
025600970626if  1c     cplatb        IFNE      *blanks
025700970624     C                   MOVEL     'N'           WRECOK
025800970626e   1c                   ENDIF
025900000726     C*
026000000726     C* obsoleto
026100000726if  1c     cplfi1        IFEQ      'O '
026200000726     C                   MOVEL     'N'           WRECOK
026300000726e   1c                   ENDIF
026400970917     C*
026500970917     C* località con sinonimi, esclude se richiesto
026600970917if  1c                   IF        parlss = 'S'
026700970917if  2c                   IF        cpless = 'S'
026800970917     C                   MOVEL     'N'           WRECOK
026900970917e   2c                   ENDIF
027000970917e   1c                   ENDIF
027100970623     C*
027200970623     C                   ENDSR
027300000000     C*------------------------------------------------------------------------*
027400970623     C* lanpgm - Lancia pgm che da localita trova i sinonimi
027500000000     C*------------------------------------------------------------------------*
027600970623     C     lanpgm        BEGSR
027700000000     C*
027800970624     c                   MOVEL     cplloc        i88loc
027900970623     C                   CALL      'TISI88R'
028000970624     C                   PARM                    TISI88DS
028100970624     c                   movea     o88sin        sin
028200000000     C*
028300000000     C                   ENDSR
028400000000     C*------------------------------------------------------------------------*
028500970623     C* scrive - Scrive sinonimi trovati
028600000000     C*------------------------------------------------------------------------*
028700970623     C     scrive        BEGSR
028800970624     c*
028900970626do  1c                   DO        60            s                 3 0
029000970626if  2c     sin(s)        IFNE      *blanks
029100970624     c                   Z-ADD     parver        cpsver
029200970624     c                   MOVEL     cplnar        cpsnar
029300970624     c                   MOVEL     cplcap        cpscap
029400970624     c                   MOVEL     cplloc        cpsloc
029500970624     c                   MOVEL     sin(s)        cpssin
029600970624     c                   MOVEL     *blanks       cpsatb
029700970624     c                   MOVEL     *blanks       cpsftr
029800970624     c                   Z-ADD     *zeros        cpsdtr
029900970624     c     keycps        CHAIN     azcps01l                           99
030000970626if  3c     *in99         IFEQ      *on                                          *non trovato
030100970626if  4c     partla        IFEQ      'A'                                          *rich. aggiornam.
030200970624     c                   WRITE     azcps000
030300970626e   4c                   ENDIF
030400970626if  4c     parsta        IFEQ      'S'                                          *rich.sta.sin.new
030500970625     C                   EXSR      stampa                                       *stampa riga sin.
030600970626e   4c                   ENDIF
030700970625     c                   ADD       1             totsin            6 0
030800970626e   3c                   ENDIF
030900970626e   2c                   ENDIF
031000970626e   1c                   ENDDO
031100000000     C*
031200000000     C                   ENDSR
031300000000     C*------------------------------------------------------------------------*
031400970624     C* stampa - STAMPA RIGA sinonimo
031500000000     C*------------------------------------------------------------------------*
031600970624     C     stampa        BEGSR
031700000000     C*
031800000000     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
031900970626if  1C     *INOF         IFEQ      '1'
032000000000     C                   EXCEPT    TESTA
032100000000     C                   SETOFF                                       OF
032200970626e   1C                   ENDIF
032300000000     C*
032400970624     C* STAMPO RIGA sinonimo
032500970624     C                   EXCEPT    riga
032600000000     C*
032700000000     C                   ENDSR
032800970624     C*------------------------------------------------------------------------*
032900970624     C* aggloc - Aggiorna flag 'esistono sinonimi' del file località
033000970624     C*------------------------------------------------------------------------*
033100970624     C     aggloc        BEGSR
033200970624     C*
033300970625     C     parver        SETLL     azcps01L                           99        *FINE-ARCHIV
033400970625     C  N99              READ      azcps01L                               99
033500970626do   C     *IN99         DOWEQ     *off
033600970625     C     cpsver        ANDEQ     parver
033700970626if  2C     cpsatb        IFEQ      *blanks
033800970625     C                   Z-ADD     cpsver        cplver
033900970625     C                   MOVEL     cpsnar        cplnar
034000970625     C                   MOVEL     cpscap        cplcap
034100970625     C                   MOVEL     cpsloc        cplloc
034200970625     c     keycpl        CHAIN     azcpl01l                           98
034300970625     C  N98              MOVEL     'S'           cpless
034400970625     c  N98              UPDATE    azcpl000
034500970626e   2c                   ENDIF
034600970625     C                   READ      azcps01L                               99
034700970626e   1C                   ENDDO
034800970624     C*
034900970624     C                   ENDSR
035000000000     C*------------------------------------------------------------------------*
035100000000     C* *INZSR - ROUTINE INIZIALE
035200000000     C*------------------------------------------------------------------------*
035300000000     C     *INZSR        BEGSR
035400000000     C*--------------------
035500000000     C* RICEVIMENTO PARAMENTRI
035600000000     C*--------------------
035700000000     C     *ENTRY        PLIST
035800000000     C                   PARM                    KPJBA
035900950703     C                   MOVEL     KPJBU         PARAM
036000000609     C*
036100000609     C* Prime considerazioni sui parametri passati
036200000609     C                   IF        %trim(parna1)='I'
036300000609     C                   EVAL      parna1=*blanks
036400000609     C                   ENDIF
036500000609     C                   IF        %trim(parna2)='I'
036600000609     C                   EVAL      parna2=*blanks
036700000609     C                   ENDIF
036800000000     C*--------------------
036900000000     C* CHIAVI DI LETTURA
037000000000     C*--------------------
037100970626     C* azcpc01l - COMPLETA
037200970624     C     keycpc        KLIST
037300970624     C                   KFLD                    cpcver
037400970624     C                   KFLD                    cpcnar
037500970624     C                   KFLD                    cpccap
037600970626     C* azcpl01l - COMPLETA
037700970624     C     keycpl        KLIST
037800970624     C                   KFLD                    cplver
037900970624     C                   KFLD                    cplnar
038000970624     C                   KFLD                    cplcap
038100970624     C                   KFLD                    cplloc
038200970626     C* azcps01l - COMPLETA
038300970624     C     keycps        KLIST
038400970624     C                   KFLD                    cpsver
038500970624     C                   KFLD                    cpsnar
038600970624     C                   KFLD                    cpscap
038700970624     C                   KFLD                    cpsloc
038800970624     C                   KFLD                    cpssin
038900000000     C*--------------------
039000000000     C* CAMPI DI CONTROLLO
039100000000     C*--------------------
039200000000     C                   MOVEL     '0'           FINL00            1            *FINE FILE
039300000000     C                   SETON                                        OF        *OVERFLOW
039400000000     C                   Z-ADD     UDATE         DATA              6 0          *DATA
039500000000     C                   TIME                    ORA               6 0          *ORA
039600000000     C*
039700000000     C                   ENDSR
039800000000     O*------------------------------------------------------------------------*
039900000000     OPRTF198   E            TESTA          1 02
040000970624     O                                            8 'TISI86R '
040100970624     O                                           90 '*** CREAZIONE SINONI'
040200970624     O                                          110 'MI DA LOCALITA ***  '
040300000000     O                                          150 'Data:'
040400000000     O                       DATA               159 '  /  /  '
040500000000     O                                          167 'Ora:'
040600941207     O                       ORA                178 '  :  :  '
040700000000     O                                          190 'Pagina:'
040800000000     O                       PAGE          Z    198
040900000000     O*------------------------
041000970624     O          E            TESTA          1
041100970624     O                                            8 'NAZIONE:'
041200970624     O                       parna1            +  1
041300970624     O                                         +  1 '-'
041400970624     O                       parna2            +  1
041500970625     O                                         + 12 'CAP:'
041600970624     O                       parca1            +  1
041700970624     O                                         +  1 '-'
041800970624     O                       parca2            +  1
041900970625     O                                         + 12 'PROVINCIA'
042000970624     O                       parpr1            +  1
042100970624     O                                         +  1 '-'
042200970624     O                       parpr2            +  1
042300970625     O                                         + 12 'LINEA ARR:'
042400970624     O                       parln1            +  1
042500970624     O                                         +  1 '-'
042600970624     O                       parln2            +  1
042700000000     O*------------------------
042800000000     O          E            TESTA          1
042900970624     O                                            3 'NAZ'
043000970624     O                                            9 'CAP'
043100970624     O                                           24 'LOCALITA'
043200970624     O                                           62 'SINONIMO'
043300000000     O*------------------------
043400970626     O          E            TESTA          1
043500970626     O                       TES(1)              66
043600970626     O                       TES(1)             132
043700970626     O                       TES(1)             198
043800000000     O*------------------------------------------------------------------------*
043900970624     O          E            RIGA           1
044000970624     O                       cpsnar
044100970624     O                       cpscap            +  1
044200970624     O                       cpsloc            +  3
044300970624     O                       cpssin            +  3
044400000000     O*------------------------
044500000000     O          E            STAFIN           63
044600970626     O                                           30 'NUOVI SINONIMI -->'
044700970624     O                       totsin        Z     38
044800000000     O                                           76 '*** FINE STAMPA ***'
044900970626     O*------------------------------------------------------------------------*
045000970626** -TES-   (DESCRIZIONI  STAMPA)
045100970626==================================================================  1
045200970626                                                                    2
045300970626                                                                    3
045400970626                                                                    4
045500970626                                                                    5
045600970626                                                                    6
045700970626                                                                    7
045800970626                                                                    8
045900970626                                                                    9
046000970626                                                                   10
