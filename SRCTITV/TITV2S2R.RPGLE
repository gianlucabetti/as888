000100030715      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200140512     H DECEDIT('0,') DATEDIT(*DMY.)
000300140120     H dftactgrp(*no) actgrp(*caller)
000400000313     F*
000500990910     Ftivin00r  uF   E             DISK    usropn
000600140114     FTIVGD00F  O    E             DISK
000700000313     D*
000800010330     D*----------------------------------------------------
000900010330     D* DICHIARAZIOINE VARIABILI DI WRK
001000010330     D*----------------------------------------------------
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400140114     D fivabds       e ds                  extname(fivab00f)
001500140114     D tisi95ds      e ds
001600010330     D esito           s              1
001700010330     D prmlit          s             10
001800010330     D prmfir          s             10
001900010330     D wrkesito        s                   like(esito)
002000010330     D rrnum           s              6  0 INZ(*zeros)
002100030715     D*------------------
002200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
002300030715     D*------------------
002400140512     D SkSplitFLD      S             35    DIM(100)
002500030715     D SkSplitCSV      S            256    DIM(100)
002600030715     D CharCSV         S              1
002700030715     D CharTXT         S              1
002800030715     D CharNUM         S              1
002900090403     D CharSOS         S              1
003000030715     D posDa           S              3  0 INZ(*zeros)
003100030715     D posA            S              3  0 INZ(*zeros)
003200030715     D i               s              3  0 INZ(1)
003300030715     D wGiro           s              1  0 INZ(*zeros)
003400030822     D*------------------
003500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
003600030822     D*------------------
003700030822     D posDaDft        S              3  0 INZ(*zeros)
003800030822     D posADft         S              3  0 INZ(*zeros)
003900030822     D j               s              3  0 INZ(1)
004000030822     D wGiroDft        s              1  0 INZ(*zeros)
004100031201     D*------------------
004200031201     D* Costanti
004300031201     D*------------------
004400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004600010330
004700010330
004800010330
004900000913     C                   reset                   rrnum
005000990921     C                   reset                   esito
005100990921     C                   reset                   wrkesito
005200010601     C*
005300010601     C                   exsr      opeini
005400010605     C                   exsr      rwvab
005500010601     C*
005600010601     C                   seton                                        lr
005700010601
005800010601
005900010601
006000010601
006100010601     C*--------------------------------------------------------
006200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006300010601     C*--------------------------------------------------------
006400010601     C     PREELA        BEGSR
006500010601     C*
006600010601     C* SE OCCORRE SPEDIRE IN FILIALE
006700010601     C                   if        invfil <> *zeros and
006800010601     C                             flgGiro = '0'
006900010601     C*
007000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007100010601     C                   eval      flgGiro = '1'
007200010601     C*
007300010601     C                   endif
007400010601     C*
007500010601     C                   ENDSR
007600010601     C***
007700010601
007800010601
007900010601
008000010601
008100010601     C*--------------------------------------------------------
008200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008300010601     C*--------------------------------------------------------
008400010601     C     ENDELA        BEGSR
008500080222     C*
008600080222     C* Effettuo lancio TISI95 solo x chiusura
008700080222     C                   CLEAR                   TISI95DS
008800080222     C                   EVAL      I95TLA = 'C'
008900080222     C                   CALL      'TISI95R'
009000080222     C                   PARM                    TISI95DS
009100000613     C*
009200010601     C                   ENDSR
009300010601     C***
009400010330
009500010601
009600010601
009700010601
009800010330     C*--------------------------------------------------------
009900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
010000010330     C*--------------------------------------------------------
010100010605     C     RWVAB         BEGSR
010200010330     C*
010300010330     C                   if        not %open(tivin00r)
010400010330     C                   open      tivin00r
010500010330     C                   endif
010600010330     C*
010700010604     C                   clear                   §CTROK
010800010604     C                   clear                   §CTRMO
010900010604     C                   clear                   §CTRNO
011000010330     C*
011100030822     C                   do        *HIVAL
011200010330     C*
011300030822     C                   read      tivin00r                               70
011400010618     C*
011500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011600010618     C                   if        vinflg = '1'
011700010618     C                   eval      flgOk = '1'
011800010618     C                   endif
011900010618     C*
012000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
012100040510     C                   z-add     *zeros        wLenVINDTA        4 0
012200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
012300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
012400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
012500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
012600040510     C                   z-add     1             wFlgVINDTA
012700040510     C                   leave
012800040510     C                   endif
012900040510     C                   enddo
013000040510     C*
013100040218     C                   if        vindta > *blanks AND
013200040510     C                             wFlgVINDTA = 1
013300010330     C                   add       1             rrnum
013400010330     C*
013500010601     C                   if        *in70 = *off and
013600010330     C                             (vinflg = *blanks
013700010330     C                              or vinflg = '0'
013800010330     C                              or vinflg = '2')
013900010330     C*
014000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014100010711     C                   if        vinflg = *blanks or vinflg = '0'
014200010711     C                   clear                   vinmsg
014300010711     C                   endif
014400010601     C*
014500010330     C                   exsr      inzvar
014600100419     C                   exsr      normCSV
014700090403     C*
014800090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
014900090403     C* separatori campi
015000090403     C                   if        CharTXT <> *blanks
015100090403     C                   z-add     *zeros        wLenVINDTA        4 0
015200090403     C                   z-add     *zeros        wFlgVINDTA        1 0
015300090403     C                   setoff                                       80
015400090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
015500090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
015600090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
015700090403     C                   if        not *in80
015800090403     C                   seton                                        80
015900090403     C                   else
016000090403     C                   setoff                                       80
016100090403     C                   endif
016200090403     C                   endif
016300090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
016400090403     C                             and *in80 = *on
016500090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
016600090403     C                   endif
016700090403     C                   enddo
016800090403     C                   endif
016900090403     C*
017000010605     C                   exsr      impvab
017100010601     C*
017200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017300010605     C                   eval      depfil = VABLNP
017400010601     C                   exsr      repfil
017500060331     C                   if        VABFGS = *zeros
017600010601     C                   if        depfil = invfil
017700021025     C                   if        vlrpoi = 999
017800030822     C                   move(P)   invfil        VABFGS
017900021025     C                   else
018000030822     C                   move(P)   vlrpoi        VABFGS
018100021025     C                   endif
018200050113     C                   endif
018300060331     C                   endif
018400010601     C*
018500010601     C                   exsr      PREELA
018600010601     C*
018700010604     C* Ebbene...
018800010604     C*
018900030822     C  N31              add       1             §CTROK            7 0
019000030822     C   32              add       1             §CTRMO            7 0
019100030822     C   31              add       1             §CTRNO            7 0
019200140114     C                   exsr      wrivgd
019300010604     C*
019400010604     C                   if        *in31 = *off and
019500010604     C                             *in32 = *off
019600010604     C                   eval      vinflg = '1'
019700010604     C                   else
019800010604     C                   eval      vinflg = '2'
019900010604     C                   endif
020000010601     C*
020100010604     C                   endif
020200010604     C*
020300010330     C                   else
020400010330     C                   eval      vinflg = '1'
020500010330     C                   endif
020600010601     C*
020700010601     C  N70              update    tivin000
020800010330     C*
020900030822     C  N70              enddo
021000010601     C
021100010601     C*
021200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
021300010601     C                   if        cntNonEl = *zeros or
021400010601     C                             flgMulti = '0'
021500010330     C* Se non ci sono record con errori ...
021600010601     C                   if        §ctrno = 0 and
021700010604     C                             §ctrmo = 0 and
021800010601     C                             flgStato <> '2'
021900010330     C* ... restituisco esito OK.
022000010330     C                   eval      wrkesito = '0'
022100010330     C                   else
022200010330     C                   if        §ctrok > 0
022300010330     C                   eval      wrkesito = '1'
022400010330     C                   else
022500010615     C                   if        flgOk = '0'
022600010615     C                   eval      wrkesito = '2'
022700010615     C                   else
022800010615     C                   eval      wrkesito = '6'
022900010615     C                   endif
023000010330     C                   endif
023100010330     C                   endif
023200010601     C                   else
023300010601     C                   eval      wrkesito = '9'
023400010601     C                   endif
023500010330     C*
023600010330     C                   if        %open(tivin00r)
023700010330     C                   close     tivin00r
023800010330     C                   endif
023900010601     C*
024000010601     C                   if        vlrpoi <> 999
024100010601     C                   eval      invfil = vlrpoi
024200010601     C                   endif
024300010601     C*
024400010618     C                   if        flgGiro = '1'
024500010601     C                   exsr      endela
024600010618     C                   endif
024700010330     C*
024800010330     C                   ENDSR
024900010330     C***
025000140114
025100140114
025200140114
025300140114      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
025400140114     C     wriVGD        BEGSR
025500140710     C*
025600140710     C                   select
025700140710     C                   when      %trim(wVABEMD) <> *blanks
025800140710     C                   eval      wVABTNO = '1'
025900140710     C                   endsl
026000140114     C*
026100140114     C                   clear                   tivgd000
026200140114     C*
026300140114     C                   if        wGiro = 1
026400140512     C                   eval      vgdDTA = 'VABCCM;VABLNP;VABCTR;VABNCL;' +
026500140512     C                                      'VABPKB;VABCBO;VABRMA;VABRSD;' +
026600140512     C                                      'VABRD2;VABIND;VABLOD;VABCAD;' +
026700140512     C                                      'VABPRD;VABCEL;VABEMD;VABTNO'
026800140114     C                   else
026900140512     C                   eval      vgdDTA = %editc(vabCCM:'X') + CharCSV +
027000140512     C                                      %editc(vabLNP:'X') + CharCSV +
027100140512     C                                      %editc(vabCTR:'X') + CharCSV +
027200140512     C                                      %editc(vabNCL:'X') + CharCSV +
027300140512     C                                      %editc(vabPKB:'3') + CharCSV +
027400140512     C                                       %trim(vabCBO)     + CharCSV +
027500140512     C                                       %trim(vabRMA)     + CharCSV +
027600140512     C                                       %trim(vabRSD)     + CharCSV +
027700140512     C                                       %trim(vabRD2)     + CharCSV +
027800140512     C                                       %trim(vabIND)     + CharCSV +
027900140512     C                                       %trim(vabLOD)     + CharCSV +
028000140512     C                                       %trim(vabCAD)     + CharCSV +
028100140512     C                                       %trim(vabPRD)     + CharCSV +
028200140512     C                                       %trim(wVABCEL)    + CharCSV +
028300140512     C                                       %trim(wVABEMD)    + CharCSV +
028400140512     C                                       %trim(wVABTNO)
028500140114     C                   endif
028600140114     C*
028700140512     C                   eval      vgdTIP = 'EB'
028800140114     C                   eval      vgdKSU = vlrKSC
028900140114     C                   eval      vgdTSC = 'WW'
029000140114     C                   eval      vgdDAT = datcor
029100140512     C                   eval      vgdPGM = 'TITV2S2R'
029200140114     C                   write     tivgd000
029300140114     C*
029400140114     C                   ENDSR
029500140114     C*------------------------------------------------------------------------*
029600140114
029700010601
029800010601
029900010601
030000010330     C*----------------------------------------------------*
030100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030200010330     C*----------------------------------------------------*
030300010330     C     INZVAR        BEGSR
030400010330     C*
030500030822     C* Inizializzo variabili di wrk
030600020204     C                   Z-ADD     *zeros        Num5_0            5 0
030700090403     C                   z-add     *zeros        invfil
030800030822     C*
030900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
031000140114     C                   CLEAR                   FIVABDS
031100030822     C                   CLEAR                   SkSplitCSV
031200140512     C                   clear                   wVABCEL          35
031300140512     C                   clear                   wVABEMD          35
031400140710     C                   movel     '0'           wVABTNO           1
031500030822     C                   EVAL      i = 1
031600030822     C                   EVAL      posDa = *zeros
031700030822     C                   EVAL      posA  = *zeros
031800030822     C*
031900030822     C* Reimposto i valori di default
032000030822     C                   EXSR      DEFCAM
032100030822     C*
032200010330     C                   ENDSR
032300010330     C*----------------------------------------------------*
032400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
032500010330     C*----------------------------------------------------*
032600010330     C     DEFCAM        BEGSR
032700110706     C*
032800110706     C                   SETOFF                                       70
032900030822     C*
033000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
033100030715     C* e delimitatore testo.
033200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
033300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
033400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
033500090403     C*
033600090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
033700090403     C                   EVAL      CharSOS = CharNUM
033800030822     C*
033900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
034000030715     C                   IF        %subst(vlrppt:1:1) = 'S'
034100030715     C                   SETON                                        50
034200030715     C                   ELSE
034300030715     C                   SETOFF                                       50
034400030715     C                   ENDIF
034500030822     C*
034600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
034700030822     C                   EVAL      posDaDft = 1
034800030822     C                   EVAL      posADft  = 0
034900030822     C                   EVAL      wGiroDft = 0
035000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
035100030822     C                             posDaDft > 0
035200030822     C*
035300030822     C* Gestisco il 1° giro
035400030822     C                   IF        wGiroDft = 0
035500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
035600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
035700030822     C* Incremento il contatore dei "giri"
035800030822     C                   EVAL      wGiroDft = 1
035900030822     C                   ELSE
036000030822     C                   EVAL      posDaDft = posADft
036100030822     C                   ENDIF
036200030822     C* Eseguo lo scan x trovare la fine del campo corrente
036300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
036400030822     C*
036500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
036600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
036700030822     C                   IF        posDaDft > 0 AND
036800030822     C                             posADft  > 0
036900110706     C* RMN
037000110706     C                   IF        %subst(
037100110706     C                             %subst(vlrppt:posDaDft+1:
037200110706     C                             posADft-posDaDft-1):1:3)
037300110706     C                             = 'RMN'
037400110706     C                   IF        %trim(%subst(
037500110706     C                             %subst(vlrppt:posDaDft+1:
037600110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037700110706     C                             = '*NSP'
037800110706     C                   SETON                                        70
037900110706     C                   ENDIF
038000110706     C                   ENDIF
038100060331     C* FGS
038200060331     C                   IF        %subst(
038300060331     C                             %subst(vlrppt:posDaDft+1:
038400060331     C                             posADft-posDaDft-1):1:3)
038500060331     C                             = 'FGS'
038600060331     C                   EVAL      PiStr=%trim(%subst(
038700060331     C                             %subst(vlrppt:posDaDft+1:
038800060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038900060331     C                   EXSR      CHKNUM
039000060331     C                   IF        PiInt=*on
039100060331     C                   Z-ADD     PiVal         VABFGS
039200060331     C                   ENDIF
039300060331     C                   ENDIF
039400030822     C* NCL
039500030822     C                   IF        %subst(
039600030822     C                             %subst(vlrppt:posDaDft+1:
039700030822     C                             posADft-posDaDft-1):1:3)
039800030822     C                             = 'NCL'
039900030822     C                   EVAL      PiStr=%trim(%subst(
040000030822     C                             %subst(vlrppt:posDaDft+1:
040100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040200030822     C                   EXSR      CHKNUM
040300030822     C                   IF        PiInt=*on
040400030822     C                   Z-ADD     PiVal         VABNCL
040500030822     C                   ENDIF
040600030822     C                   ENDIF
040700030822     C* CCM
040800030822     C                   IF        %subst(
040900030822     C                             %subst(vlrppt:posDaDft+1:
041000030822     C                             posADft-posDaDft-1):1:3)
041100030822     C                             = 'CCM'
041200030822     C                   EVAL      PiStr=%trim(%subst(
041300030822     C                             %subst(vlrppt:posDaDft+1:
041400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041500030822     C                   EXSR      CHKNUM
041600030822     C                   IF        PiInt=*on
041700030822     C                   Z-ADD     PiVal         VABCCM
041800030822     C                   ENDIF
041900030822     C                   ENDIF
042000030822     C* LNP
042100030822     C                   IF        %subst(
042200030822     C                             %subst(vlrppt:posDaDft+1:
042300030822     C                             posADft-posDaDft-1):1:3)
042400030822     C                             = 'LNP'
042500030822     C                   EVAL      PiStr=%trim(%subst(
042600030822     C                             %subst(vlrppt:posDaDft+1:
042700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042800030822     C                   EXSR      CHKNUM
042900030822     C                   IF        PiInt=*on
043000030822     C                   Z-ADD     PiVal         VABLNP
043100030822     C                   ENDIF
043200030822     C                   ENDIF
043300030822     C* NRS
043400030822     C                   IF        %subst(
043500030822     C                             %subst(vlrppt:posDaDft+1:
043600030822     C                             posADft-posDaDft-1):1:3)
043700030822     C                             = 'NRS'
043800030822     C                   EVAL      PiStr=%trim(%subst(
043900030822     C                             %subst(vlrppt:posDaDft+1:
044000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044100030822     C                   EXSR      CHKNUM
044200030822     C                   IF        PiInt=*on
044300030822     C                   Z-ADD     PiVal         VABNRS
044400030822     C                   ENDIF
044500030822     C                   ENDIF
044600030822     C* CTR
044700030822     C                   IF        %subst(
044800030822     C                             %subst(vlrppt:posDaDft+1:
044900030822     C                             posADft-posDaDft-1):1:3)
045000030822     C                             = 'CTR'
045100030822     C                   EVAL      PiStr=%trim(%subst(
045200030822     C                             %subst(vlrppt:posDaDft+1:
045300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045400030822     C                   EXSR      CHKNUM
045500030822     C                   IF        PiInt=*on
045600030822     C                   Z-ADD     PiVal         VABCTR
045700030822     C                   ENDIF
045800030822     C                   ENDIF
045900030822     C* PKB
046000030822     C                   IF        %subst(
046100030822     C                             %subst(vlrppt:posDaDft+1:
046200030822     C                             posADft-posDaDft-1):1:3)
046300030822     C                             = 'PKB'
046400030822     C                   EVAL      PiStr=%trim(%subst(
046500030822     C                             %subst(vlrppt:posDaDft+1:
046600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046700030822     C                   EXSR      CHKNUM
046800030822     C                   IF        PiNum=*on
046900030822     C                   Z-ADD     PiVal         VABPKB
047000030822     C                   ENDIF
047100030822     C                   ENDIF
047200030822     C* VLB
047300030822     C                   IF        %subst(
047400030822     C                             %subst(vlrppt:posDaDft+1:
047500030822     C                             posADft-posDaDft-1):1:3)
047600030822     C                             = 'VLB'
047700030822     C                   EVAL      PiStr=%trim(%subst(
047800030822     C                             %subst(vlrppt:posDaDft+1:
047900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048000030822     C                   EXSR      CHKNUM
048100030822     C                   IF        PiNum=*on
048200030822     C                   Z-ADD     PiVal         VABVLB
048300030822     C                   ENDIF
048400030822     C                   ENDIF
048500030822     C* QFT
048600030822     C                   IF        %subst(
048700030822     C                             %subst(vlrppt:posDaDft+1:
048800030822     C                             posADft-posDaDft-1):1:3)
048900030822     C                             = 'QFT'
049000030822     C                   EVAL      PiStr=%trim(%subst(
049100030822     C                             %subst(vlrppt:posDaDft+1:
049200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049300030822     C                   EXSR      CHKNUM
049400030822     C                   IF        PiNum=*on
049500030822     C                   Z-ADD     PiVal         VABQFT
049600030822     C                   ENDIF
049700030822     C                   ENDIF
049800030822     C* CBO
049900030822     C                   IF        %subst(
050000030822     C                             %subst(vlrppt:posDaDft+1:
050100030822     C                             posADft-posDaDft-1):1:3)
050200030822     C                             = 'CBO'
050300030822     C                   EVAL      VABCBO=%trim(%subst(
050400030822     C                             %subst(vlrppt:posDaDft+1:
050500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050600030822     C                   ENDIF
050700030822     C* TSP
050800030822     C                   IF        %subst(
050900030822     C                             %subst(vlrppt:posDaDft+1:
051000030822     C                             posADft-posDaDft-1):1:3)
051100030822     C                             = 'TSP'
051200030822     C                   EVAL      VABTSP=%trim(%subst(
051300030822     C                             %subst(vlrppt:posDaDft+1:
051400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051500030822     C                   ENDIF
051600030822     C* VAS
051700030822     C                   IF        %subst(
051800030822     C                             %subst(vlrppt:posDaDft+1:
051900030822     C                             posADft-posDaDft-1):1:3)
052000030822     C                             = 'VAS'
052100030822     C                   EVAL      VABVAS=%trim(%subst(
052200030822     C                             %subst(vlrppt:posDaDft+1:
052300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052400030822     C                   ENDIF
052500030822     C* VCA
052600030822     C                   IF        %subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):1:3)
052900030822     C                             = 'VCA'
053000030822     C                   EVAL      VABVCA=%trim(%subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300030822     C                   ENDIF
053400030822     C* GCA
053500030822     C                   IF        %subst(
053600030822     C                             %subst(vlrppt:posDaDft+1:
053700030822     C                             posADft-posDaDft-1):1:3)
053800030822     C                             = 'GCA'
053900030822     C                   EVAL      VABGCA=%trim(%subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054200030822     C                   ENDIF
054300030822     C* CTM
054400030822     C                   IF        %subst(
054500030822     C                             %subst(vlrppt:posDaDft+1:
054600030822     C                             posADft-posDaDft-1):1:3)
054700030822     C                             = 'CTM'
054800030822     C                   EVAL      VABCTM=%trim(%subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055100030822     C                   ENDIF
055200030822     C* FFD
055300030822     C                   IF        %subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):1:3)
055600030822     C                             = 'FFD'
055700030822     C                   EVAL      VABFFD=%trim(%subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000030822     C                   ENDIF
056100030822     C* VAD
056200030822     C                   IF        %subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):1:3)
056500030822     C                             = 'VAD'
056600030822     C                   EVAL      VABVAD=%trim(%subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900030822     C                   ENDIF
057000030822     C* GMA
057100030822     C                   IF        %subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):1:3)
057400030822     C                             = 'GMA'
057500030822     C                   EVAL      VABGMA=%trim(%subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800030822     C                   ENDIF
057900030822     C* GGA
058000030822     C                   IF        %subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):1:3)
058300030822     C                             = 'GGA'
058400030822     C                   EVAL      VABGGA=%trim(%subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058700030822     C                   ENDIF
058800030822     C* GVA
058900030822     C                   IF        %subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):1:3)
059200030822     C                             = 'GVA'
059300030822     C                   EVAL      VABGVA=%trim(%subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600030822     C                   ENDIF
059700030822     C* TC1
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'TC1'
060200030822     C                   EVAL      VABTC1=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   ENDIF
060600030822     C* TC2
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'TC2'
061100030822     C                   EVAL      VABTC2=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   ENDIF
061500030822     C* ...
061600030822     C                   ENDIF
061700030822     C                   ENDDO
061800020204     C*
061900010330     C                   ENDSR
062000010607     C*----------------------------------------------------*
062100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
062200010607     C*----------------------------------------------------*
062300010607     C     IMPVAB        BEGSR
062400010607     C*
062500010607     C                   SETOFF                                       3132
062600030515     C*
062700010607     C                   MOVEL     datcor        VABAAS
062800010607     C                   MOVE      datcor        VABMGS
062900010607     C*
063000030715     C**********
063100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
063200030715     C**********
063300030715     C                   IF        wGiro = *zeros
063400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
063500031201     C     minu:maiu     XLATE     vindta        vindta
063600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
063700040324     C                   DOW       posDa <= %len(%trimr(vindta))
063800030715     C*
063900030715     C* Gestisco il 1° campo
064000030715     C                   IF        i = 1
064100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
064200030715     C                   EVAL      posDa = 1
064300030715     C* Eseguo lo scan x trovare la fine del primo campo
064400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
064500030715     C* A questo punto "estraggo" il campo corrente
064600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
064700030715     C                                                    (posA-posDa))
064800030715     C* X i campi successivi al 1°
064900030715     C                   ELSE
065000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
065100030715     C                   EVAL      posDa = posA + 1
065200030715     C* Eseguo lo scan x trovare la fine del campo corrente
065300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
065400030715     C* Gestisco l'ultimo campo
065500030715     C                   IF        posA = *zeros
065600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
065700030715     C* A questo punto "estraggo" il campo corrente
065800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
065900030715     C                                                    (posA-posDa))
066000030715     C                   LEAVE
066100030715     C                   ELSE
066200030715     C* A questo punto "estraggo" il campo corrente
066300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
066400030715     C                                                    (posA-posDa))
066500030715     C                   ENDIF
066600030715     C                   ENDIF
066700030715     C* Incremento il contatore d campo
066800030715     C                   EVAL      i = i +1
066900030715     C                   ENDDO
067000030715     C*
067100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
067200030715     C* delimitazione testo
067300030715     C                   EVAL      i = 1
067400030715     C                   DOW       i < %elem(SkSplitFLD)
067500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
067600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
067700030715     C                   EVAL      i = i + 1
067800030715     C                   ENDDO
067900030715     C                   EVAL      wGiro = 1
068000030715     C                   ELSE
068100030715     C                   EVAL      wGiro = 2
068200031204     C**********
068300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
068400031204     C* d separatore campo
068500031204     C**********
068600040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
068700040304     C                   EVAL      vindta = ' ' + vindta
068800040304     C                   ENDIF
068900040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
069000040304     C***                EVAL      vindta = %subst(vindta:2)
069100040304     C***                ENDDO
069200031204     C*
069300031204     C                   Z-ADD     *zeros        lunghInput        4 0
069400040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
069500040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
069600040304     C                   EVAL      vindta = vindta + ' '
069700040304     C                   ENDIF
069800040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
069900040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
070000040304     C***                EVAL      lunghInput = %len(%trim(vindta))
070100040304     C***                ENDDO
070200030715     C**********
070300030715     C* Effettuo lo split del campo dati d input
070400030715     C**********
070500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
070600040324     C                   DOW       posDa <= %len(%trimr(vindta))
070700030715     C*
070800030715     C* Gestisco il 1° campo
070900030715     C                   IF        i = 1
071000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
071100030715     C                   EVAL      posDa = 1
071200030715     C* Eseguo lo scan x trovare la fine del primo campo
071300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071400030715     C* A questo punto "estraggo" il campo corrente
071500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
071600030715     C                                                    (posA-posDa))
071700030715     C* X i campi successivi al 1°
071800030715     C                   ELSE
071900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
072000030715     C                   EVAL      posDa = posA + 1
072100031204     C* Verifico che nn vi sia il campo nn valorizzato
072200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
072300031204     C* Se campo nn valorizzato skippo al prossimo
072400031204     C                   EVAL      SkSplitCSV(i) = *blanks
072500031204     C                   EVAL      posA  = posA + 1
072600031204     C                   ELSE
072700030715     C* Eseguo lo scan x trovare la fine del campo corrente
072800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072900030715     C* Gestisco l'ultimo campo
073000030715     C                   IF        posA = *zeros
073100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
073200030715     C* A questo punto "estraggo" il campo corrente
073300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
073400030715     C                                                    (posA-posDa))
073500030715     C                   LEAVE
073600030715     C                   ELSE
073700030715     C* A questo punto "estraggo" il campo corrente
073800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
073900030715     C                                                    (posA-posDa))
074000030715     C                   ENDIF
074100031204     C                   ENDIF
074200030715     C                   ENDIF
074300030715     C* Incremento il contatore d campo
074400030715     C                   EVAL      i = i +1
074500030715     C                   ENDDO
074600030715     C*
074700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
074800030715     C* delimitazione testo
074900030715     C                   EVAL      i = 1
075000030715     C                   DOW       i < %elem(SkSplitCSV)
075100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
075200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
075300030715     C                   EVAL      i = i + 1
075400030715     C                   ENDDO
075500030715     C*
075600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
075700030715     C                   EVAL      i = 1
075800030715     C                   DOW       i < %elem(SkSplitFLD)
075900030715     C*
076000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
076100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
076200030715     C                             SkSplitCSV(i) <> *blanks
076300130624     C*** ATB
076400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
076500130624     C                   IF        SkSplitCSV(i) = 'D'
076600130624     C                   EVAL      VABCTR = 390
076700130624     C                   EVAL      VABCTM = '7R'
076800130624     C                   ENDIF
076900030721     C                   ENDIF
077000030715     C*** RSD
077100140512     C                   IF        %trim(SkSplitFLD(i)) = 'RECIPIENTCOMPANY'
077200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
077300030715     C                   ENDIF
077400140512     C*
077500140512     C                   IF        %trim(SkSplitFLD(i)) = 'CUSTOMERID'
077600140512     C                   EVAL      VABRSD = %trim(VABRSD) + ' ' + SkSplitCSV(i)
077700140512     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
077800140512     C     '@':'A'       XLATE     VABRSD        VABRSD
077900140512     C* ==
078000140512     C                   ENDIF
078100030721     C*** RD2
078200140512     C                   IF        %trim(SkSplitFLD(i)) = 'RECIPIENTNAME'
078300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
078400030721     C                   ENDIF
078500030715     C*** IND
078600140512     C                   IF        %trim(SkSplitFLD(i)) = 'ADDRESS1'
078700030715     C                   EVAL      VABIND = SkSplitCSV(i)
078800030715     C                   ENDIF
078900140512     C*
079000140512     C                   IF        %trim(SkSplitFLD(i)) = 'ADDRESS2'
079100140512     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
079200140512     C                   ENDIF
079300030715     C*** LOD
079400140512     C                   IF        %trim(SkSplitFLD(i)) = 'CITY'
079500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
079600030715     C                   ENDIF
079700140512     C*** wVABEMD
079800140512     C                   IF        %trim(SkSplitFLD(i)) = 'RECIPIENTEMAIL'
079900140512     C                   EVAL      wVABEMD = SkSplitCSV(i)
080000140512     C                   ENDIF
080100140512     C*** wVABCEL
080200140512     C                   IF        %trim(SkSplitFLD(i)) = 'PHONE'
080300140512     C                   EVAL      wVABCEL = SkSplitCSV(i)
080400140512     C                   ENDIF
080500030715     C*** PRD
080600140512     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
080700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
080800030715     C                   ENDIF
080900030721     C*** NZD
081000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
081100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
081200030721     C                   ENDIF
081300030715     C*** RMA
081400140512     C                   IF        %trim(SkSplitFLD(i)) = 'ORDERID'
081500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
081600030715     C                   ENDIF
081700030715     C*** NOT
081800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
081900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
082000030715     C                   ENDIF
082100030715     C*** NT2
082200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
082300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
082400030715     C                   ENDIF
082500030715     C*** VCA
082600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
082700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
082800030715     C                   ENDIF
082900030715     C*** VAS
083000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
083100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
083200030715     C                   ENDIF
083300030715     C*** FFD
083400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
083500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
083600030715     C                   ENDIF
083700030715     C*** CBO
083800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
083900030715     C                   EVAL      VABCBO = SkSplitCSV(i)
084000030715     C                   ENDIF
084100030721     C*** GC1
084200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
084300120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
084400120314     C                   MOVEL     *blanks       wGC               1
084500120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
084600120314     C                   IF        SkSplitCSV(i) = 'M' OR
084700120314     C                             SkSplitCSV(i) = 'P'
084800120314     C                   MOVE(P)   wGC           VABGC1
084900120314     C                   ELSE
085000120314     C                   MOVEL(P)  wGC           VABGC1
085100120314     C                   ENDIF
085200120314     C                   ENDIF
085300030721     C                   ENDIF
085400030721     C*** GC2
085500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
085600120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
085700120314     C                   MOVEL     *blanks       wGC               1
085800120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
085900120314     C                   IF        SkSplitCSV(i) = 'M' OR
086000120314     C                             SkSplitCSV(i) = 'P'
086100120314     C                   MOVE(P)   wGC           VABGC2
086200120314     C                   ELSE
086300120314     C                   MOVEL(P)  wGC           VABGC2
086400120314     C                   ENDIF
086500120314     C                   ENDIF
086600030721     C                   ENDIF
086700030721     C*** TSP
086800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
086900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
087000030721     C                   ENDIF
087100030721     C*** NAS
087200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
087300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
087400030721     C                   ENDIF
087500030721     C*** TIC
087600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
087700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
087800030721     C                   ENDIF
087900030721     C*** GCA
088000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
088100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
088200030721     C                   ENDIF
088300030721     C*** XCO
088400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
088500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
088600030721     C                   ENDIF
088700030721     C*** CTM
088800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
088900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
089000030721     C                   ENDIF
089100030721     C*** TCR
089200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
089300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
089400030721     C                   ENDIF
089500030721     C*** CTS
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
089700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030721     C*** FTM
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
090100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030721     C*** VAD
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
090500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030721     C*** GMA
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
090900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030721     C*** GGA
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
091300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
091400030721     C                   ENDIF
091500030721     C*** GVA
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
091700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
091800030721     C                   ENDIF
091900030721     C*** TC1
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
092100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030721     C*** TC2
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
092500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
092600030721     C                   ENDIF
092700030721     C*** SCL
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
092900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030721     C*** RMO
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
093300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030721     C*** NMO
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
093700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030721     C*
094000070705     C* Reperisco quindi i campi numerici...
094100030721     C*** CAD
094200140512     C                   IF        %trim(SkSplitFLD(i)) = 'ZIP'
094300070705     C                   EVAL      VABCAD = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030721     C*** RMN
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
094700030721     C                   EVAL      PiStr=SkSplitCSV(i)
094800030721     C                   EXSR      CHKNUM
094900030721     C                   IF        PiInt=*on
095000030721     C                   Z-ADD     PiVal         VABRMN
095100030721     C                   ELSE
095200030721     C                   SETON                                        32
095300030721     C                   EVAL      VABRMN = 1
095400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095500030721     C                             + ' ' + 'VABRMN'
095600030721     C                   ENDIF
095700030721     C                   ENDIF
095800030721     C*** NSP
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
096000030721     C                   EVAL      PiStr=SkSplitCSV(i)
096100030721     C                   EXSR      CHKNUM
096200100517     C                   IF        PiInt=*on
096300030721     C                   Z-ADD     PiVal         VABNSP
096400030721     C                   ELSE
096500030721     C                   SETON                                        32
096600030721     C                   EVAL      VABNSP = *zeros
096700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096800030721     C                             + ' ' + 'VABNSP'
096900030721     C                   ENDIF
097000030721     C                   ENDIF
097100030721     C*** NCL
097200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
097300030721     C                   EVAL      PiStr=SkSplitCSV(i)
097400030721     C                   EXSR      CHKNUM
097500030721     C                   IF        PiInt=*on
097600030721     C                   Z-ADD     PiVal         VABNCL
097700030721     C                   ELSE
097800030721     C                   SETON                                        32
097900030721     C                   EVAL      VABNCL = *zeros
098000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098100030721     C                             + ' ' + 'VABNCL'
098200030721     C                   ENDIF
098300030721     C                   ENDIF
098400030721     C*** PKB
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
098600030721     C                   EVAL      PiStr=SkSplitCSV(i)
098700030721     C                   EXSR      CHKNUM
098800030721     C                   IF        PiNum=*on
098900030721     C                   Z-ADD     PiVal         VABPKB
099000030721     C                   ELSE
099100030721     C                   SETON                                        32
099200030721     C                   EVAL      VABPKB = *zeros
099300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099400030721     C                             + ' ' + 'VABPKB'
099500030721     C                   ENDIF
099600030721     C                   ENDIF
099700030721     C*** CAS
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
099900030721     C                   EVAL      PiStr=SkSplitCSV(i)
100000030721     C                   EXSR      CHKNUM
100100030721     C                   IF        PiNum=*on
100200030721     C                   Z-ADD     PiVal         VABCAS
100300030721     C                   ELSE
100400030721     C                   SETON                                        32
100500030721     C                   EVAL      VABCAS = *zeros
100600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100700030721     C                             + ' ' + 'VABCAS'
100800030721     C                   ENDIF
100900030721     C                   ENDIF
101000030721     C*** CCM
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
101200030721     C                   EVAL      PiStr=SkSplitCSV(i)
101300030721     C                   EXSR      CHKNUM
101400030721     C                   IF        PiInt=*on      AND
101500030721     C                             PiVal<=9999999 AND
101600030721     C                             PiVal>*zeros
101700030721     C                   Z-ADD     PiVal         VABCCM
101800030721     C                   ELSE
101900030721     C                   SETON                                        32
102000030721     C                   EVAL      VABCCM = *zeros
102100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102200030721     C                             + ' ' + 'VABCCM'
102300030721     C                   ENDIF
102400030721     C                   ENDIF
102500030721     C*** LNP
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
102700030721     C                   EVAL      PiStr=SkSplitCSV(i)
102800030721     C                   EXSR      CHKNUM
102900030721     C                   IF        PiInt=*on    AND
103000030721     C                             PiVal<=999   AND
103100030721     C                             PiVal>*zeros
103200030721     C                   Z-ADD     PiVal         VABLNP
103300030721     C                   ELSE
103400030721     C                   SETON                                        32
103500030721     C                   EVAL      VABLNP = *zeros
103600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103700030721     C                             + ' ' + 'VABLNP'
103800030721     C                   ENDIF
103900030721     C                   ENDIF
104000030721     C*** NRS
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
104200030721     C                   EVAL      PiStr=SkSplitCSV(i)
104300030721     C                   EXSR      CHKNUM
104400030721     C                   IF        PiInt=*on AND
104500030721     C                             PiVal<=99
104600030721     C                   Z-ADD     PiVal         VABNRS
104700030721     C                   ELSE
104800030721     C                   SETON                                        32
104900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105000030721     C                             + ' ' + 'VABNRS'
105100030721     C                   ENDIF
105200030721     C                   ENDIF
105300030721     C*** LNA
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
105500120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
105600120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
105700120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
105800120314     C                             %trim(SkSplitCSV(i)) <> *blanks
105900030721     C                   EVAL      PiStr=SkSplitCSV(i)
106000030721     C                   EXSR      CHKNUM
106100030721     C                   IF        PiInt=*on    AND
106200030721     C                             PiVal<=999   AND
106300030721     C                             PiVal>*zeros
106400030721     C                   Z-ADD     PiVal         VABLNA
106500030721     C                   ELSE
106600030721     C                   SETON                                        32
106700030721     C                   EVAL      VABLNA = *zeros
106800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106900030721     C                             + ' ' + 'VABLNA'
107000120314     C                   ENDIF
107100030721     C                   ENDIF
107200030721     C                   ENDIF
107300030721     C*** CTR
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
107500030721     C                   EVAL      PiStr=SkSplitCSV(i)
107600030721     C                   EXSR      CHKNUM
107700030721     C                   IF        PiInt=*on  AND
107800030721     C                             PiVal<=999
107900030721     C                   Z-ADD     PiVal         VABCTR
108000030721     C                   ELSE
108100030721     C                   SETON                                        32
108200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108300030721     C                             + ' ' + 'VABCTR'
108400030721     C                   ENDIF
108500030721     C                   ENDIF
108600030721     C*** IAS
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
108800030721     C                   EVAL      PiStr=SkSplitCSV(i)
108900030721     C                   EXSR      CHKNUM
109000030721     C                   IF        PiNum=*on
109100030721     C                   Z-ADD     PiVal         VABIAS
109200030721     C                   ELSE
109300030721     C                   SETON                                        32
109400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109500030721     C                             + ' ' + 'VABIAS'
109600030721     C                   ENDIF
109700030721     C                   ENDIF
109800030721     C*** VLB
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiNum=*on
110300030721     C                   Z-ADD     PiVal         VABVLB
110400030721     C                   ELSE
110500030721     C                   SETON                                        32
110600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110700030721     C                             + ' ' + 'VABVLB'
110800030721     C                   ENDIF
110900030721     C                   ENDIF
111000030721     C*** QFT
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiNum=*on
111500030721     C                   Z-ADD     PiVal         VABQFT
111600030721     C                   ELSE
111700030721     C                   SETON                                        32
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900030721     C                             + ' ' + 'VABQFT'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** NCD
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiInt=*on      AND
112700030721     C                             PiVal<=9999999
112800030721     C                   Z-ADD     PiVal         VABNCD
112900030721     C                   ELSE
113000030721     C                   SETON                                        32
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200030721     C                             + ' ' + 'VABNCD'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** NCA
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiInt=*on      AND
114000030721     C                             PiVal<=9999999
114100030721     C                   Z-ADD     PiVal         VABNCA
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114500030721     C                             + ' ' + 'VABNCA'
114600030721     C                   ENDIF
114700030721     C                   ENDIF
114800030721     C*** ZNC
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiInt=*on AND
115300030721     C                             PiVal<=99
115400030721     C                   Z-ADD     PiVal         VABZNC
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800030721     C                             + ' ' + 'VABZNC'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** DCR
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on       AND
116600030721     C                             PiVal<=99999999
116700030721     C                   Z-ADD     PiVal         VABDCR
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100030721     C                             + ' ' + 'VABDCR'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** HCR
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiInt=*on   AND
117900030721     C                             PiVal<=9999
118000030721     C                   Z-ADD     PiVal         VABHCR
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABHCR'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** VMD
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiNum=*on
119200030721     C                   Z-ADD     PiVal         VABVMD
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABVMD'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** ANT
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on        AND
120400030721     C                             PiVal<=999999999
120500030721     C                   Z-ADD     PiVal         VABANT
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABANT'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** CMO
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on
121700030721     C                   Z-ADD     PiVal         Num5_0
121800030721     C                   MOVEL(p)  Num5_0        VABCMO
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      VABCMO = *zeros
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABCMO'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030715     C*
122700030715     C                   ENDIF
122800030715     C                   EVAL      i = i + 1
122900030715     C                   ENDDO
123000070705     C*
123100070705     C* Considerazioni particolari
123200110706     C*
123300110706     C*** RMN
123400110706     C                   IF        *in70
123500110706     C                   EVAL      VABRMN = VABNSP
123600110706     C                   ENDIF
123700070705     C*** NZD
123800070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
123900070705     C                   EVAL      VABNZD = *blanks
124000070705     C                   ENDIF
124100070705     C*** CAD
124200070705     C                   IF        VABNZD = *blanks
124300070705     C                   EVAL      PiStr=VABCAD
124400070705     C                   EXSR      CHKNUM
124500070705     C                   IF        PiInt=*on
124600070705     C                   Z-ADD     PiVal         Num5_0
124700070705     C                   MOVEL(p)  Num5_0        VABCAD
124800070705     C                   ELSE
124900070705     C                   SETON                                        32
125000070705     C                   EVAL      VABCAD = *zeros
125100070705     C                   EVAL      vinmsg = %trimr(vinmsg)
125200070705     C                             + ' ' + 'VABCAD'
125300070705     C                   ENDIF
125400070705     C                   ENDIF
125500080222     C*
125600080222     C* Se provincia nn valorizzata la reperisco
125700080222     C* tramite TISI95R a seconda dei dati d instradamento presenti
125800080222     C                   IF        VABPRD = *blanks
125900080222     C                   CLEAR                   TISI95DS
126000080222     C                   EVAL      I95TCN = '3'
126100080222     C                   Z-ADD     datcor        I95DAT
126200080222     C                   EVAL      I95NAR = VABNZD
126300080222     C                   EVAL      I95CAP = VABCAD
126400080222     C                   EVAL      I95LOC = VABLOD
126500080222     C                   CALL      'TISI95R'
126600080222     C                   PARM                    TISI95DS
126700080222     C                   EVAL      VABPRD = O95PRV
126800080222     C                   ENDIF
126900020204     C*
127000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
127100020204     C                   EXSR      CHKIMPDIV
127200030715     C                   ENDIF
127300020204     C*
127400010607     C                   ENDSR
127500010607     C*----------------------------------------------------*
127600010601
127700020204
127800020204     C*----------------------------------------------------*
127900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
128000020204     C*----------------------------------------------------*
128100020204     C     CHKIMPDIV     BEGSR
128200060524     C*
128300060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
128400060524     C                   IF        vabCAS > *zeros
128500060524     C                   IF        vabCBO = '1'
128600060524     C                   EVAL      vabCBO = '4'
128700060524     C                   ENDIF
128800060524     C                   IF        vabCBO = '2'
128900060524     C                   EVAL      vabCBO = '6'
129000060524     C                   ENDIF
129100060524     C                   ENDIF
129200020204     C*
129300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
129400020204     C                   Z-ADD     *zeros        wrkDec            9 9
129500020204     C*
129600020204     C* Come prima cosa effettuo considerazioni sulla divisa
129700020204     C                   IF        vabIAS > *zeros
129800020204     C                   IF        vabVAS <> 'EUR'
129900020204     C                   EVAL      vabVAS =  'ITL'
130000020204     C                   ENDIF
130100020204     C                   ENDIF
130200020204     C*
130300020204     C                   IF        vabCAS > *zeros
130400020204     C                   IF        vabVCA <> 'EUR'
130500020204     C                   EVAL      vabVCA =  'ITL'
130600020204     C                   ENDIF
130700020204     C                   ENDIF
130800020204     C*
130900020204     C                   IF        vabVMD > *zeros
131000020204     C                   IF        vabVAD <> 'EUR'
131100020204     C                   EVAL      vabVAD =  'ITL'
131200020204     C                   ENDIF
131300020204     C                   ENDIF
131400020204     C*
131500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
131600020204     C                   Z-ADD     vabIAS        wrkDec
131700020204     C                   IF        wrkDec > *zeros
131800020204     C                   IF        vabVAS = 'ITL'
131900020204     C                   EVAL      vabIAS = *zeros
132000020204     C                   ENDIF
132100020204     C                   ENDIF
132200020204     C*
132300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
132400020204     C                   Z-ADD     vabCAS        wrkDec
132500020204     C                   IF        wrkDec > *zeros
132600020204     C                   IF        vabVCA = 'ITL'
132700020204     C                   EVAL      vabCAS = *zeros
132800020204     C                   ENDIF
132900020204     C                   ENDIF
133000020204     C*
133100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
133200020204     C                   Z-ADD     vabVMD        wrkDec
133300020204     C                   IF        wrkDec > *zeros
133400020204     C                   IF        vabVAD = 'ITL'
133500020204     C                   EVAL      vabVMD = *zeros
133600020204     C                   ENDIF
133700020204     C                   ENDIF
133800020204     C*
133900020204     C                   ENDSR
134000020204     C***
134100020204
134200010330
134300010330
134400010330     C*----------------------------------------------------*
134500010330     C*  CONTROLLO NUMERICITA' CAMPI
134600010330     C*----------------------------------------------------*
134700010330     C     CHKNUM        BEGSR
134800010330     C*
134900010606     C                   IF        PiDecChr = *blanks
135000030715     C                   EVAL      PiDecChr = CharNUM
135100010606     C                   ENDIF
135200010606     C*
135300010606     C                   CALL(e)   'ISNUMERIC'
135400010330     C                   PARM                    PiStr            30
135500010606     C                   PARM                    PiDecChr          1
135600010330     C                   PARM      *ZEROS        PiVal            30 9
135700010330     C                   PARM      '0'           PiInt             1
135800010330     C                   PARM      '0'           PiNum             1
135900010330     C                   IF        %error
136000010606     C                   EVAL      PiNum=*off
136100010330     C                   ENDIF
136200010330     C*
136300010330     C                   ENDSR
136400010330     C***
136500010330
136600010601
136700010601
136800010601
136900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
137000010601     C     repfil        BEGSR
137100010601     C*
137200010601     C                   if        invfil = *zeros and
137300010601     C                             depfil > *zeros and
137400010629     C                             (vinflg = *blanks or
137500010629     C                              vinflg = *zeros)
137600010601     C
137700010601     C                   eval      invfil = depfil
137800010601     C                   endif
137900010601     C*
138000010601     C                   if        depfil <> invfil and
138100010601     C                             invfil > *zeros
138200010601     C                   eval      flgMulti = '1'
138300010601     C                   if        vinflg = *blanks
138400010601     C                   add       1             cntNonEl
138500010601     C                   endif
138600010601     C                   endif
138700010601     C*
138800010601     C                   if        vinflg = '2'
138900010601     C                   eval      flgStato = '2'
139000010601     C                   endif
139100010601     C*
139200010601     C                   ENDSR
139300010601     C***
139400010601
139500010601
139600010601
139700010601      /TITLE Invio dei dati al punto operativo.
139800010601     C     opeini        BEGSR
139900010601     C*
140000010601     C* Inizializzo flag e contatori operativi
140100010601     C                   movel     '0'           flgGiro           1
140200010601     C                   movel     '0'           flgMulti          1
140300010601     C                   movel     '1'           flgStato          1
140400010615     C                   movel     '0'           flgOk             1
140500010601     C                   z-add     *zeros        cntNonEl         10 0
140600010601     C                   z-add     *zeros        depfil            3 0
140700010601     C                   z-add     *zeros        invfil            3 0
140800010601     C*
140900010601     C                   ENDSR
141000010601     C***
141100050421
141200050421
141300050421
141400050421     C     *pssr         BEGSR
141500050421     C*
141600050421     C                   eval      wrkesito = '2'
141700050421     C*
141800050421     C                   ENDSR     '*CANCL'
141900050421     C***
142000100419
142100100419
142200100419
142300100419     C     normCSV       BEGSR
142400100419     C*
142500100419     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
142600100419     C* separatori campi
142700100419     C                   if        CharTXT <> *blanks
142800100419     C                   z-add     *zeros        wLenVINDTA        4 0
142900100419     C                   z-add     *zeros        wFlgVINDTA        1 0
143000100419     C                   setoff                                       80
143100100419     C                   dow       wLenVINDTA < %len(%trim(vindta))
143200100419     C                   eval      wLenVINDTA = wLenVINDTA + 1
143300100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
143400100419     C                   if        not *in80
143500100419     C                   seton                                        80
143600100419     C                   else
143700100419     C                   setoff                                       80
143800100419     C                   endif
143900100419     C                   endif
144000100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
144100100419     C                             and *in80 = *on
144200100419     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
144300100419     C                   endif
144400100419     C                   enddo
144500100419     C                   endif
144600100419     C*
144700100419     C                   ENDSR
144800100419     C***
144900010601
145000010330
145100010330
145200000613     C     *inzsr        BEGSR
145300990910     C*
145400990910     C     *entry        plist
145500990920     C                   parm                    tivlrds
145600990921     C                   parm      wrkesito      esito
145700000724     C                   parm                    prmlit
145800000710     C                   parm                    prmfir
145900010330     C*
146000010330     C* CALCOLA LA DATA CORRENTE
146100140512     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
146200140512     C                   eval      datcor = %dec(%date() : *iso)
146300000613     C*
146400000613     C                   ENDSR
146500000613     C***
