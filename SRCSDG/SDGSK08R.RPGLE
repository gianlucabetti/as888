000100981211     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981211     H*PARMS BNDDIR(PJXBND PJCBND)
000300981027     H*PARMS CVTOPT(*DATETIME)
000400981027     H DECEDIT('0,') DATEDIT(*DMY.)
000500920514     F*---------------------------------------------------------------*H
000600160616     F* SDGSK08R  *      SCRITTURA ARCHIVIO IMPORTI CASELLE MOD. 770  *
000700160616     F*                  QUADRO SY                                    *
000800920514     F*---------------------------------------------------------------*
000900930909     F*   D A T A    B A S E                                          *
001000930909     F*---------------------------------------------------------------*
001100160616     FSDGN7707L IF   E           K DISK
001200160616     FND77010F  O  A E             DISK
001300160616     F*-
001400160616     D F0D             S            200    DIM(9)
001500160616     D SCK             S             24    DIM(75)
001600160616     D CAM             S              1    DIM(16)
001700160616     D C15T            S             15    DIM(03)
001800160616     D*------------
001900160616     D DatMin          C                   CONST('0001-01-01')
002000160616     D*------------
002100160616     D KPJBA         E DS
002200160616     D*------------
002300160617     D GGDS          E DS                  EXTNAME(ND77000IDS) INZ
002400160616     D*-------------
002500160616     D ndcr80ds      E DS
002600160616     D*-------------
002700160616     D* Dati di ambiente ottenuti da XSOC
002800160616     D SOC001        E DS                  EXTNAME(SDGSOCDS )
002900160616     D*-------------
003000160616     D* DS Interna per dati di output di XSOC
003100160616     D XSOCDS          DS          1000
003200160616     D*-------------
003300160616     D AMG             DS
003400160616     D  AMGAnno                1      4
003500160616     D  T1                     5      5
003600160616     D  AMGMese                6      7
003700160616     D  T2                     8      8
003800160616     D  AMGGiorno              9     10
003900160616     D*-------------
004000160616     D                 DS
004100160616     D  PEGIO                  1      2  0
004200160616     D  PEMES                  3      4  0
004300160616     D  PEANN                  5      8  0
004400160616     D  DATNAS                 1      8  0
004500160616     D                 DS
004600160616     D  QUADRO                 1      2
004700160616     D  RIGO                   3      5  0
004800160617     D  COLO                   6      8  0
004900160616     D  CODICE                 1      8
005000160616     D                 DS
005100160616     D  CAMPO                  1     16
005200160616     D  C15I                  17     61
005300160616     D    C15                        15    Dim(03) Overlay(C15I)
005400160616     D  C60                    1     61
005500160616     D*-------------
005600160616     D CampoWrk        DS
005700160616     D CampoInt                1     14
005800160616     D CampoDec               15     16
005900160616     D*
006000160616     D CampoIntero     DS
006100160616     D  Int                    1     13
006200160616     D  virgola               14     14
006300160616     D  Dec                   15     16
006400160616     D*-------------
006500160616     D SavCdfs         S                   Like(N77Cdfs)
006600160616     D SavEventi       S                   Like(N77Eventi)
006700160617     D Import          S             13  2
006800160617     D Importz         S             13  0
006900160616     C*----------------------------------------------------*
007000160616     C*- Operazioni iniziali e scrittura Angrafica Azienda
007100160616     C                   Exsr      INZVAR
007200160616     C*
007300160616     C* Posizionamento con
007400160616     C                   MOVEL     PER1R80       ANNO              4 0
007500160616     C                   Z-ADD     ANNO          N77ACF
007600160616     C                   MOVEL     'SY'          N77QUA
007700160616     C     KEY770        SETLL     SDGN7707L
007800160616     C     KEY770        READE     SDGN7707L                              21
007900160616     C  N21              Exsr      SalvaKey
008000160616     C*
008100160616     C                   DoW       *In21 = *off
008200160616     C*
008300160617     C* A rottura codice fiscale, evento oppure per max righe
008400160616     C                   If        N77Cdfs<> SavCdfs   Or
008500160617     C                             N77Eventi <> SavEventi or
008600160617     C                             ScRig = 20
008700160616     C*   scarico il record
008800160616     C                   Exsr      WrSc
008900160616     C*   salvo la nuova chiave
009000160616     C                   Exsr      SalvaKey
009100160617     C*
009200160616     C                   Movel     '        '    SCK(1)
009300160616     C                   Z-Add     0             Sc
009400160616     C                   Z-Add     0             ScCol
009500160617     C                   Z-Add     1             ScRig
009600160616     C                   Add       1             ScMod
009700160616     C                   ENDIF
009800160616     C*
009900160617     C* Scrittura Anagrafiche e importi percipienti esteri
010000160617     C                   Exsr      WRTANI
010100160616     C*
010200160617     C     KEY770        READE     SDGN7707l                              21
010300160616     C                   EndDo
010400160616     C*
010500160616     C     SC            CASGT     1             WRSC
010600160616     C                   ENDCS
010700160616     C*
010800160616     C     FINE          TAG
010900160616     C                   SETON                                        LR
011000160616     C*----------------------------------------------------*
011100160616     C* SALVATAGGIO CHIAVE PER ROTTURA
011200160616     C*----------------------------------------------------*
011300160616     C     SALVAKEY      BEGSR
011400160616     C*
011500160616     C                   Move      N77CDFS       SavCdfs
011600160616     C                   Move      N77EVENTI     SavEventi
011700160616     C                   ENDSR
011800160616     C*----------------------------------------------------*
011900160616     C*-
012000160616     C* SCRITTURA RECORD FILE TRACCIATO MINISTERIALE
012100160616     C*-
012200160616     C*----------------------------------------------------*
012300160617     C     WRTANI        BEGSR
012400160616     C*
012500160616     C*              -------------------
012600160617     C*              SCRITTURA QUADRO SY
012700160616     C*              -------------------
012800160616     C*
012900160617     C                   MOVEL     'SY'          QUADRO
013000160616     C*
013100160616     C                   If        SCRIG         = 999
013200160617     C                   Z-Add     1             SCRIG
013300160616     C                   EndIf
013400160616     C*
013500160616     C* solo all'inizio del record
013600160617     C                   If        SCRIG         = 1
013700160617     C* Codice fiscale del sostituto di imposta
013800160617     C                   Z-ADD     1             SCCOL
013900160617     C                   MOVEL     N77CDFS       C60
014000160616     c                   if        c60           <> *blanks
014100160616     C                   Exsr      WSC
014200160616     c                   endif
014300160617     C* Eventi Eccezionali
014400160617     C                   ADD       1             SCCOL
014500160617     C                   MOVE      N77EVENTI     CAMPO
014600160617     c                   if        c60           <> *blanks
014700160617     C                   Exsr      WSC
014800160617     C                   endif
014900160617     C*
015000160617     C                   Z-add     15            SCRIG
015100160617     C*
015200160617     C                   Endif
015300160616     C* RAGIONE SOCIALE - COGNOME
015400160617     C                   ADD       1             SCRIG
015500160617     C                   Z-ADD     1             SCCOL
015600160617     C                   MOVEL     N77COG        C60
015700160617     C                   Exsr      WSC
015800160616     C* NOME
015900160616     C                   ADD       1             SCCOL
016000160616     C     N77PFG        IFEQ      'F'
016100160616     C                   MOVEL     N77NOM        C60
016200160616     C                   ENDIF
016300160616     c                   if        c60           <> *blanks
016400160616     C                   Exsr      WSC
016500160616     c                   endif
016600160616     C* SESSO
016700160616     C                   ADD       1             SCCOL
016800160616     C                   MOVEL     N77SES        C60
016900160616     c                   if        c60           <> *blanks
017000160616     C                   Exsr      WSC
017100160616     c                   endif
017200160616     C* DATA DI NASCITA
017300160616     C                   ADD       1             SCCOL
017400160616     C                   IF        N77PFG        = 'F'
017500160616     C                   IF        N77DNC        <> DatMin
017600160616     C                   MOVE      N77DNC        AMG
017700160616     C                   MOVE      AMGAnno       PEANN
017800160616     C                   MOVE      AMGMese       PEMES
017900160616     C                   MOVE      AMGGiorno     PEGIO
018000160616     C                   Else
018100160616     C                   MOVE      *zeros        Datnas
018200160616     C                   Endif
018300160617     c                   If        DatNas <> 0
018400160616     C                   MOVE      DATNAS        CAMPO
018500160617     C                   EndIf
018600160616     C                   ElSe
018700160616     C                   MOVE      *blanks       CAMPO
018800160616     C                   Endif
018900160617     c                   if        campo         <> *blanks  and
019000160617     c                             campo         <> *zeros
019100160616     c                   if        c60           <> *blanks
019200160616     C                   Exsr      WSC
019300160616     c                   endif
019400160617     c                   endif
019500160617     C* Identificativo fiscale estero
019600160617     C                   ADD       1             SCCOL
019700160617     C                   MOVEL     N77CDE        C60
019800160617     c                   If        c60           <> *blanks
019900160617     C                   Exsr      WSC
020000160617     c                   EndIf
020100160616     C* LOCALITA' DI RESIDENZA ESTERA
020200160616     C                   ADD       1             SCCOL
020300160617     C                   MOVEL     N77CIT        C60
020400160616     c                   if        c60           <> *blanks
020500160616     C                   Exsr      WSC
020600160616     c                   endif
020700160616     C* INDIRIZZO ESTERO
020800160616     C                   ADD       1             SCCOL
020900160617     C                   MOVEL     N77VIA        C60
021000160616     c                   if        c60           <> *blanks
021100160616     C                   Exsr      WSC
021200160616     c                   endif
021300160616     C* COD. STATO ESTERO
021400160616     C                   ADD       1             SCCOL
021500160616     C                   MOVEL     N77CSE        CODEST            3
021600160616     C                   MOVE      CODEST        CAMPO
021700160616     c                   if        c60           <> *blanks
021800160616     C                   Exsr      WSC
021900160616     c                   endif
022000160616     C*
022100160616     C*
022200160616     C* CAUSALE
022300160620     C                   ADD       1             SCCOL
022400160616     C                   MOVEL     N77C77        C60
022500160617     C                   if        c60           <> *blanks
022600160616     C                   Exsr      WSC
022700160617     C                   endif
022800160617     C* AMMONTARE LORDO CORRISPOSTO
022900160616     C                   ADD       1             SCCOL
023000160616     C                   z-add     N77ITC        IMPORT
023100160617     C     TroncR80      IfEq      *On
023200160617     C                   Z-Add     IMPORT        IMPORTZ          13 0
023300160617     C                   MOVE      IMPORTZ       CAMPO
023400160617     C                   Else
023500160617     C                   MOVE      IMPORT        CAMPO
023600160617     C                   EXSR      WSCINT
023700160617     C                   EndIf
023800160617     C                   Exsr      ABLANK
023900160617     c                   if        c60           <> *blanks
024000160616     C                   Exsr      WSC
024100160616     c                   endif
024200160617     C* SOMME NON SOGGETTE A RITENUTA PER REGIME CONVENZIONALE
024300160617     C                   SETOFF                                       2122
024400160617     C     N77RAC        COMP      0                                      21
024500160617     C     N77IMN        COMP      0                                  22
024600160616     C* SOMME NON SOGGETTE A RIT. PER REGIME CONVENZIONALE
024700160616     C                   Z-ADD     0             IMPORT
024800160616     C                   ADD       1             SCCOL
024900160616     C* STRANIERO SENZA RITENUTE E CON IMP. NON SOGGETTTO
025000160617     C   21
025100160617     CAn 22N77IMN        ADD       N77SNS        IMPORT
025200160617     C     TroncR80      IfEq      *On
025300160617     C                   Z-Add     IMPORT        IMPORTZ
025400160617     C                   MOVE      IMPORTZ       CAMPO
025500160617     C                   Else
025600160617     C                   MOVE      IMPORT        CAMPO
025700160617     C                   EXSR      WSCINT
025800160617     C                   EndIf
025900160616     C                   Exsr      ABLANK
026000160616     c                   if        c60           <> *blanks
026100160616     C                   Exsr      WSC
026200160617     C                   endif
026300160617     C
026400160617     C* ALTRE SOMME NON SOGGETTE A RITENUTA
026500160616     C                   Z-ADD     0             IMPORT
026600160616     C                   ADD       1             SCCOL
026700160616     C* STRANIERO SENZA RITENUTE SENZA IMP. NON SOGGETTO
026800160617     C   21
026900160616     CANN22N77IMN        ADD       N77SNS        IMPORT
027000160616     C* STRANIERO CON RITENUTE
027100160617     C  N21N77IMN        ADD       N77SNS        IMPORT
027200160617     C     TroncR80      IfEq      *On
027300160617     C                   Z-Add     IMPORT        IMPORTZ
027400160617     C                   MOVE      IMPORTZ       CAMPO
027500160617     C                   Else
027600160616     C                   MOVE      IMPORT        CAMPO
027700160617     C                   EXSR      WSCINT
027800160617     C                   EndIf
027900160616     C                   Exsr      ABLANK
028000160616     c                   if        c60           <> *blanks
028100160616     C                   Exsr      WSC
028200160616     c                   endif
028300160617     C* IMPONIBILE
028400160616     C                   ADD       1             SCCOL
028500160616     C                   z-add     N77IMS        Import
028600160617     C     TroncR80      IfEq      *On
028700160617     C                   Z-Add     IMPORT        IMPORTZ
028800160617     C                   MOVE      IMPORTZ       CAMPO
028900160617     C                   Else
029000160616     C                   MOVE      Import        CAMPO
029100160617     C                   EXSR      WSCINT
029200160617     C                   EndIf
029300160616     C                   Exsr      ABLANK
029400160617     C                   if        c60           <> *blanks
029500160616     C                   Exsr      WSC
029600160617     C                   endif
029700160617     C*
029800160617     C* RITENUTE A TITOLO D'IMPOSTA
029900160617     C                   ADD       1             SCCOL
030000160617     C                   Z-ADD     N77RIm        Import
030100160617     C*
030200160617     C                   if        TroncR80    = *On
030300160617     C                   Z-Add     Import        ImportZ
030400160617     C                   Move      ImportZ       Campo
030500160617     C                   else
030600160617     C                   Move      Import        Campo
030700160617     C                   EXSR      WSCINT
030800160617     C                   endif
030900160617     C*
031000160617     C                   Exsr      ABlank
031100160617     C                   if        C60        <> *Blanks
031200160617     C                   Exsr      WSC
031300160617     C                   endif
031400160617     C*
031500160617     C* RITENUTE SOSPESE
031600160616     C                   ADD       1             SCCOL
031700160616     C                   z-add     N77NET        IMPORT
031800160617     C     TroncR80      IfEq      *On
031900160617     C                   Z-Add     IMPORT        IMPORTZ
032000160617     C                   MOVE      IMPORTZ       CAMPO
032100160617     C                   Else
032200160616     C                   MOVE      IMport        CAMPO
032300160617     C                   EXSR      WSCINT
032400160617     C                   EndIf
032500160617     C                   Exsr      ABLANK
032600160617     c                   if        c60           <> *blanks
032700160616     C                   Exsr      WSC
032800160616     c                   endif
032900160616     C*
033000160617     C     SCk(1)        IFeq      *blanks
033100160617     C                   Z-ADD     1             SCRIG
033200160617     C                   ADD       1             SCMOD
033300160617     C*
033400160616     C                   ENDIF
033500160616     C*
033600160616     C                   ENDSR
033700160616     C*----------------------------------------------------------------
033800160616     C*
033900160616     C* TOGLIE ZERI NON SIGNIFICATIVI DAL CAMPO
034000160616     C*
034100160616     C*----------------------------------------------------------------
034200160616     C     ABLANK        BEGSR
034300160616     C*
034400160616     C                   MOVEA     CAMPO         CAM
034500160616     C*
034600160616     C                   DO        16            X                 2 0
034700160616     C     CAM(X)        IFEQ      ' '
034800160616     C     CAM(X)        OREQ      '0'
034900160616     C                   MOVE      ' '           CAM(X)
035000160616     C                   ELSE
035100160616     C                   LEAVE
035200160616     C                   ENDIF
035300160616     C                   ENDDO
035400160617     C*
035500160616     C                   If        Cam(13)       = ' '  and
035600160616     C                             Cam(14)       = ','  and
035700160616     C                             Cam(15)       <>' '
035800160616     C                   Eval      Cam(13)       = '0'
035900160617     C                   EndIf
036000160616     C*
036100160616     C                   MOVEA     CAM           CAMPO
036200160616     C*
036300160616     C                   ENDSR
036400160616     C*----------------------------------------------------------------
036500160616     C*
036600160617     C* PREPARA SCHIERA QUADRO SY
036700160616     C*
036800160616     C*----------------------------------------------------------------
036900160616     C     WSC           BEGSR
037000160616     C*
037100160616     C                   Z-ADD     SCCOL         COLO
037200160616     C                   Z-ADD     SCRIG         RIGO
037300160616     C                   ADD       1             SC
037400160616     C                   MOVEL     CODICE        SCK(SC)
037500160616     C                   MOVE      CAMPO         SCK(SC)
037600160616     C                   MOVEL     *BLANK        CAMPO
037700160616     C*
037800160616     C* SE RIEMPIO LA SCHIERA CREO RECORD
037900160616     C     SC            CASEQ     75            WRSC
038000160616     C                   ENDCS
038100160616     C*
038200160616     C* CONTROLLO SE CI SONO DATI DOPO LA 17° POSIZIONE
038300160616     C                   DO        3             XY                1 0
038400160616     C                   If        C15(XY) <> *blANKS
038500160616     C                   ADD       1             SC
038600160616     C                   MOVEL     CODICE        SCK(SC)
038700160616     C                   MOVEL     '+'           C16              16
038800160616     C                   MOVE      C15(XY)       C16
038900160616     C                   MOVE      C16           SCK(SC)
039000160616     C     SC            CASEQ     75            WRSC
039100160616     C                   ENDCS
039200160616     C                   ENDIF
039300160616     C                   ENDDO
039400160616     C*
039500160616     C                   Clear                   C60
039600160616     C*
039700160616     C* SE RAGGIUNGO IL NUMERO MAX DI RIGHE INCREMENTO MODULO
039800160617     C*
039900160617     C**** SCRIG         IFEQ      6
040000160617     C**** SCCOL         ANDEQ     28
040100160617     C****               Exsr      WRSC
040200160617     C****               Z-ADD     0             SCRIG
040300160617     C****               ADD       1             SCMOD
040400160617     C****               Z-add     0             SC
040500160617     C****               MOVEL     '        '    SCK(1)
040600160617     C****               ENDIF
040700160616     C*
040800160616     C                   ENDSR
040900160617     C*----------------------------------------------------------------
041000160617     C* Tratta campo importo
041100160617     C*----------------------------------------------------------------
041200160617     C     WSCINT        BEGSR
041300160617     C*
041400160617     C                   If        Import        <> 0
041500160617     C                   Move      Campo         CampoWRk
041600160617     C                   Move      CampoInt      Int
041700160617     C                   Move      ','           Virgola
041800160617     C                   Move      CampoDec      Dec
041900160617     C                   Move      CampoIntero   Campo
042000160617     C                   EndIf
042100160617     C*
042200160617     C                   ENDSR
042300160616     C*----------------------------------------------------------------
042400160616     C*
042500160616     C* SCRIVE RECORD SC
042600160616     C*
042700160616     C*----------------------------------------------------------------
042800160616     C     WRSC          BEGSR
042900160616     C*
043000160617     C                   MOVE      'I'           M710TR
043100160616     C                   MOVEL     XSCCDF        M710CF
043200160616     C                   Z-ADD     SCMOD         M710PM
043300160617     C                   Clear                   M710CP
043400160616     C                   MOVEA     SCK           F0D
043500160616     C                   MOVEA     F0D(1)        F01
043600160616     C                   MOVEA     F0D(2)        F02
043700160616     C                   MOVEA     F0D(3)        F03
043800160616     C                   MOVEA     F0D(4)        F04
043900160616     C                   MOVEA     F0D(5)        F05
044000160616     C                   MOVEA     F0D(6)        F06
044100160616     C                   MOVEA     F0D(7)        F07
044200160616     C                   MOVEA     F0D(8)        F08
044300160616     C                   MOVEA     F0D(9)        F09
044400160616     C                   MOVE      GGDS          M71REC
044500160616     C                   WRITE     M77010
044600160616     C                   Z-ADD     0             SC
044700160616     C                   CLEAR                   GGDS
044800160616     C                   CLEAR                   SCK
044900160616     C                   CLEAR                   F0D
045000160616     C                   CLEAR                   M71REC
045100160616     C*
045200160616     C                   ENDSR
045300160616     C*----------------------------------------------------*
045400160616     C* SUBROUTINE OPERAZIONI INIZIALI E SCRITTURA         *
045500160616     C* ARCHIVIO ANAGRAFICA AZIENDA                        *
045600160616     C*----------------------------------------------------*
045700160616     C     INZVAR        BEGSR
045800160616     C*-
045900160616     C     *ENTRY        PLIST
046000160616     C                   PARM                    KPJBA
046100160616     C*
046200160616     C                   movel     kpjbu         ndcr80ds
046300160616     C* --
046400160616     C     KEY770        KLIST
046500160616     C                   KFLD                    XSCSOC
046600160616     C                   KFLD                    N77ACF
046700160616     C                   KFLD                    N77QUA
046800160616     C*-
046900160616     C* Chiamata a xsoc
047000160616     C                   move      socR80        SOCXSC
047100160616     C                   MOVEL     'SOC001'      TIPXSC
047200160616     C                   CALL      'SDGSOCDR'
047300160616     C                   PARM                    TIPXSC            6
047400160616     C                   PARM                    SOCXSC            3
047500160616     C                   PARM                    CDSXSC            9 0
047600160616     C                   PARM                    MODXSC            3
047700160616     C                   PARM      *BLANKS       RTNXSC            1
047800160616     C                   PARM                    XSOCDS
047900160616     C                   PARM                    KPJBA
048000160616     C     RTNXSC        IFNE      '1'
048100160616     C                   MOVEL     XSOCDS        SOC001
048200160616     C                   ELSE
048300160616     C                   GOTO      FINE
048400160616     C                   ENDIF
048500160616     C                   MOVE      *BLANK        CAMPO            16
048600160616     C                   MOVE      *BLANK        C60
048700160616     C                   MOVEL     '        '    SCK(1)
048800160616     C*
048900160616     C* CAMPI QUADRO SY
049000160616     C*
049100160617     C                   Z-ADD     1             SCRIG             3 0
049200160616     C                   Z-ADD     0             SCCOL             2 0
049300160616     C                   Z-ADD     0             SC                3 0
049400160616     C                   Z-ADD     1             SCMOD             8 0
049500160616     C*
049600160616     C                   ENDSR
