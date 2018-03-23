000100080314      /TITLE Upload via Internet: traduzione in TIESV00F
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600080314     Ftiesv00f  uF A E             DISK
000700010330     D*----------------------------------------------------
000800070124     D* DICHIARAZIONE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D psds           sds
001100010330     D  procname         *PROC
001200010330     D tivlrds       e ds                  extname(tivlr00f)
001300120316     D desv          e ds                  inz
001400010330     D esito           s              1
001500080314     D prmlit          s             10
001600080314     D prmfir          s             10
001700010330     D wrkesito        s                   like(esito)
001800010330     D wrkdata         s               d
001900010330     D wrkora          s               t
002000080314     D rrnum           s              6  0 INZ(*zeros)
002100080314     D*------------------
002200080314     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
002300080314     D*------------------
002400080314     D InStringa       S          10240    VARYING                              (stringa input)
002500080314     D InSepara        S             10                                         (separatore)
002600080314     D skSplitCSV      S           4096    DIM(1000) VARYING
002700080314     D lenInSepara     S              2  0 INZ(*zeros)
002800080314     D posDa           S              5  0 INZ(*zeros)
002900080314     D posA            S              5  0 INZ(*zeros)
003000080314     D i               s              4  0 INZ(1)
003100080314     D wSize           S             10I 0 INZ(*zeros)
003200080314     D*-------------------
003300080314     D* VARIABILI DI WRK
003400080314     D*-------------------
003500080314     D wCharCSV        s              8    inz('|')
003600080314     D wAlfa           s           4096    inz varying
003700080314     D wRiga           s            198    inz
003800060906     D*------------------
003900080109     C*
004000990915     C                   time                    wrkdata
004100990915     C                   time                    wrkora
004200000913     C                   reset                   rrnum
004300990921     C                   reset                   esito
004400990921     C                   reset                   wrkesito
004500010601     C*
004600010601     C                   exsr      opeini
004700060803     C                   exsr      rwfile
004800010601     C*
004900060905     C                   seton                                        LR
005000010601     C***
005100010330     C*--------------------------------------------------------
005200060803     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIARG                 *
005300010330     C*--------------------------------------------------------
005400060803     C     RWFILE        BEGSR
005500010330     C*
005600010330     C                   if        not %open(tivin00r)
005700010330     C                   open      tivin00r
005800010330     C                   endif
005900010330     C*
006000010604     C                   clear                   §CTROK
006100010604     C                   clear                   §CTRNO
006200060720     C*
006300060720     C                   exsr      defcam
006400010330     C*
006500080109    1C                   DO        *HIVAL
006600010330     C*
006700010330     C                   READ      tivin00r                               70
006800010618     C*
006900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
007000080109    2C                   if        vinflg = '1'
007100010618     C                   eval      flgOk = '1'
007200080109    2C                   endif
007300010618     C*
007400080109    2C                   if        vindta > *blanks
007500010330     C                   add       1             rrnum
007600010330     C*
007700080109    3C                   if        *in70 = *off and
007800010330     C                             (vinflg = *blanks
007900010330     C                              or vinflg = '0'
008000010330     C                              or vinflg = '2')
008100010330     C*
008200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
008300080109    4C                   if        vinflg = *blanks or vinflg = '0'
008400010711     C                   clear                   vinmsg
008500080109    4C                   endif
008600010601     C*
008700060803     C* Esegue l'effettiva acquisizione dei dati
008800060803     C                   exsr      impfile
008900010604     C*
009000080109    3C                   endif
009100020722     C*
009200080314    3C                   if        *in31 = *off and
009300010604     C                             *in32 = *off
009400010604     C                   eval      vinflg = '1'
009500010604     C                   else
009600010604     C                   eval      vinflg = '2'
009700080109    3C                   endif
009800010604     C*
009900010330     C                   else
010000010330     C                   eval      vinflg = '1'
010100080109    2C                   endif
010200010601     C*
010300010601     C  N70              update    tivin000
010400010330     C*
010500080109    1C  N70              ENDdo
010600080314     C*
010700080314     C* Ebbene...
010800080314     C  N31              ADD       1             §CTROK            7 0
010900080314     C   31              ADD       1             §CTRNO            7 0
011000080314     C*
011100120316     C                   EVAL      ESVFLO = DESV
011200080314     C  N31              WRITE     TIESV000
011300010601     C*
011400010330     C* Se non ci sono record con errori ...
011500080314     C                   if        §ctrno = 0
011600010330     C* ... restituisco esito OK.
011700010330     C                   eval      wrkesito = '0'
011800010330     C                   else
011900010330     C                   if        §ctrok > 0
012000010330     C                   eval      wrkesito = '1'
012100010330     C                   else
012200010615     C                   if        flgOk = '0'
012300010615     C                   eval      wrkesito = '2'
012400010615     C                   else
012500010615     C                   eval      wrkesito = '6'
012600010615     C                   endif
012700010330     C                   endif
012800010330     C                   endif
012900010330     C*
013000010330     C                   if        %open(tivin00r)
013100010330     C                   close     tivin00r
013200010330     C                   endif
013300010330     C*
013400010330     C                   ENDSR
013500010330     C***
013600010601
013700010601
013800010330     C*----------------------------------------------------*
013900020722     C*  IMPOSTAZIONE CAMPI COSTANTI
014000010330     C*----------------------------------------------------*
014100010330     C     DEFCAM        BEGSR
014200080314     C*
014300080314     C                   SETOFF                                       3132
014400010330     C*
014500020204     C* Inizializzo il buffer del record da scrivere
014600080314     C                   CLEAR                   TIESV000
014700080314     C*
014800020204     C* Imposto i valori di default...
014900020204     C* ... e poi verifico se sono stati passati come parametri
015000020204     C                   IF        vlrppt > *blanks
015100020204     C                   ENDIF
015200020204     C*
015300010330     C                   ENDSR
015400010607     C*----------------------------------------------------*
015500080314     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X TIESV)
015600010607     C*----------------------------------------------------*
015700070112     C     IMPFILE       BEGSR
015800080314     C*
015900080314     C                   EVAL      ESVEDAT = DATCOR
016000080314     C*
016100080314     C* Eseguo "splittamento" dei dati
016200080314     C                   EVAL      InStringa = vindta
016300080314     C                   EVAL      InSepara  = wCharCSV
016400080314     C                   EXSR      SPLIT
016500080314     C*
016600080314     C* Imposto il buffer del file d output
016700090708     C****
016800080314     C                   IF        %trim(skSplitCSV(1)) = 'CLI'
016900080314     C                   EVAL      PiStr=%trim(skSplitCSV(2))
017000080314     C                   EXSR      CHKNUM
017100080314     C                   IF        PiInt=*on
017200080314     C                   Z-ADD     PiVal         ESVEKSC
017300080314     C                   ELSE
017400080314     C                   SETON                                        31
017500080314     C                   EVAL      vinmsg = %trimr(vinmsg)
017600080314     C                             + ' ' + 'ESVEKSC'
017700080314     C                   ENDIF
017800080314     C                   ENDIF
017900080314     C*
018000090708     C****
018100080314     C                   IF        %trim(skSplitCSV(1)) = 'VER'
018200080314     C                   EVAL      ESVEVER = %trim(skSplitCSV(2))
018300080314     C                   ENDIF
018400080314     C*
018500090708     C****
018600080314     C                   IF        %trim(skSplitCSV(1)) = 'CAP'
018700080314     C                   EVAL      PiStr=%trim(skSplitCSV(2))
018800080314     C                   EXSR      CHKNUM
018900080314     C                   IF        PiInt=*on
019000080314     C                   Z-ADD     PiVal         ESVCVER
019100080314     C                   ELSE
019200080314     C                   SETON                                        31
019300080314     C                   EVAL      vinmsg = %trimr(vinmsg)
019400080314     C                             + ' ' + 'ESVCVER'
019500080314     C                   ENDIF
019600080314     C*
019700080314     C                   EVAL      PiStr=%trim(skSplitCSV(3))
019800080314     C                   EVAL      PiStr=%subst(PiStr:7:4)+
019900080314     C                                   %subst(PiStr:4:2)+
020000080314     C                                   %subst(PiStr:1:2)
020100080314     C                   EXSR      CHKNUM
020200080314     C                   IF        PiInt=*on
020300080314     C                   Z-ADD     PiVal         ESVCDSC
020400080314     C                   ELSE
020500080314     C                   SETON                                        31
020600080314     C                   EVAL      vinmsg = %trimr(vinmsg)
020700080314     C                             + ' ' + 'ESVCDSC'
020800080314     C                   ENDIF
020900080314     C                   ENDIF
021000080314     C*
021100090708     C****
021200080314     C                   IF        %trim(skSplitCSV(1)) = 'SEG'
021300080314     C                   EVAL      PiStr=%trim(skSplitCSV(2))
021400080314     C                   EXSR      CHKNUM
021500080314     C                   IF        PiInt=*on
021600080314     C                   Z-ADD     PiVal         ESVSNCD
021700080314     C                   ELSE
021800080314     C                   SETON                                        31
021900080314     C                   EVAL      vinmsg = %trimr(vinmsg)
022000080314     C                             + ' ' + 'ESVSNCD'
022100080314     C                   ENDIF
022200080314     C*
022300080314     C                   EVAL      PiStr=%trim(skSplitCSV(3))
022400080314     C                   EXSR      CHKNUM
022500080314     C                   IF        PiInt=*on
022600080314     C                   Z-ADD     PiVal         ESVSNCA
022700080314     C                   ELSE
022800080314     C                   SETON                                        31
022900080314     C                   EVAL      vinmsg = %trimr(vinmsg)
023000080314     C                             + ' ' + 'ESVSNCA'
023100080314     C                   ENDIF
023200080314     C*
023300080314     C                   EVAL      PiStr=%trim(skSplitCSV(4))
023400080314     C                   EXSR      CHKNUM
023500080314     C                   IF        PiInt=*on
023600080314     C                   Z-ADD     PiVal         ESVSNCC
023700080314     C                   ELSE
023800080314     C                   SETON                                        31
023900080314     C                   EVAL      vinmsg = %trimr(vinmsg)
024000080314     C                             + ' ' + 'ESVSNCC'
024100080314     C                   ENDIF
024200080314     C                   ENDIF
024300080710     C*
024400090708     C****
024500080710     C                   IF        %trim(skSplitCSV(1)) = 'NRS'
024600080710     C                   EVAL      ESVNRS = %trim(skSplitCSV(2))
024700080710     C                   ENDIF
024800090604     C*
024900090708     C****
025000090604     C                   IF        %trim(skSplitCSV(1)) = 'OBJ'
025100090604     C                   EVAL      ESVOBJ = %trim(skSplitCSV(2))
025200090604     C                   ENDIF
025300090604     C*
025400090708     C****
025500090604     C                   IF        %trim(skSplitCSV(1)) = 'TXT'
025600090604     C                   EVAL      ESVCORPO = %trim(skSplitCSV(2))
025700090604     C                   ENDIF
025800090604     C*
025900090708     C****
026000090604     C                   IF        %trim(skSplitCSV(1)) = 'TYW'
026100090604     C                   EVAL      ESVTYPWEB = %trim(skSplitCSV(2))
026200090604     C                   ENDIF
026300090708     C*
026400090708     C****
026500090708     C                   IF        %trim(skSplitCSV(1)) = 'CA1'
026600090708     C                   EVAL      PiStr=%trim(skSplitCSV(2))
026700090708     C                   EXSR      CHKNUM
026800090708     C                   IF        PiInt=*on
026900090708     C                   Z-ADD     PiVal         ESVCVERMAX
027000090708     C                   ELSE
027100090708     C                   SETON                                        31
027200090708     C                   EVAL      vinmsg = %trimr(vinmsg)
027300090708     C                             + ' ' + 'ESVCVERMAX'
027400090708     C                   ENDIF
027500090708     C*
027600090708     C                   EVAL      PiStr=%trim(skSplitCSV(3))
027700090708     C                   EVAL      PiStr=%subst(PiStr:7:4)+
027800090708     C                                   %subst(PiStr:4:2)+
027900090708     C                                   %subst(PiStr:1:2)
028000090708     C                   EXSR      CHKNUM
028100090708     C                   IF        PiInt=*on
028200090708     C                   Z-ADD     PiVal         ESVCDDEMAX
028300090708     C                   ELSE
028400090708     C                   SETON                                        31
028500090708     C                   EVAL      vinmsg = %trimr(vinmsg)
028600090708     C                             + ' ' + 'ESVCDDEMAX'
028700090708     C                   ENDIF
028800090708     C*
028900090708     C                   EVAL      PiStr=%trim(skSplitCSV(4))
029000090708     C                   EVAL      PiStr=%subst(PiStr:7:4)+
029100090708     C                                   %subst(PiStr:4:2)+
029200090708     C                                   %subst(PiStr:1:2)
029300090708     C                   EXSR      CHKNUM
029400090708     C                   IF        PiInt=*on
029500090708     C                   Z-ADD     PiVal         ESVCDSCMAX
029600090708     C                   ELSE
029700090708     C                   SETON                                        31
029800090708     C                   EVAL      vinmsg = %trimr(vinmsg)
029900090708     C                             + ' ' + 'ESVCDSCMAX'
030000090708     C                   ENDIF
030100090708     C                   ENDIF
030200120316     C*
030300120316     C****
030400120316     C                   IF        %trim(skSplitCSV(1)) = 'SRV'
030500120316     C                   EVAL      D§ESVSRV = %trim(skSplitCSV(2))
030600120316     C                   ENDIF
030700020204     C*
030800080314     C                   ENDSR
030900080314
031000080314     C*------------------------------------------------------------------------*
031100080314     C* SPLIT - Routine di reperimento PORTO e VARIE fatturate
031200080314     C*------------------------------------------------------------------------*
031300080314     C     SPLIT         BEGSR
031400080314     C*
031500080314     C* Innanzitutto inizializzo i parametri d OUT
031600080314     C                   RESET                   lenInSepara
031700080314     C                   RESET                   posDa
031800080314     C                   RESET                   posA
031900080314     C                   RESET                   i
032000080314     C                   RESET                   wSize
032100080314     C*
032200080314     C* Determino subito la lunghezza del "separatore"
032300080314     C                   EVAL      lenInSepara = %len(%trim(InSepara))
032400080314     C**********
032500080314     C* Effettuo lo split della stringa dati d input
032600080314     C**********
032700080314     C*
032800080314     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in elementi
032900080314     C                   DOW       posDa <= %len(%trim(InStringa))
033000080314     C*
033100080314     C* Gestisco il 1° elemento
033200080314     C                   IF        i = 1
033300080314     C*
033400080314     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo elemen
033500080314     C                   EVAL      posDa = 1
033600080314     C*
033700080314     C* Eseguo lo scan x trovare la fine del primo campo
033800080314     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
033900080314     C                                     posDa)
034000080314     C*
034100080314     C* Verifico se il separatore è presente nella stringa d input, se nn è presente assumo che
034200080314     C* l'intero contenuto della stringa è l'unico elemento presente
034300080314     C                   IF        posA = *zeros
034400080314     C                   CLEAR                   SkSplitCSV
034500080314     C                   EVAL      SkSplitCSV(i) = %trim(InStringa)
034600080314     C                   LEAVE
034700080314     C                   ELSE
034800080314     C*
034900080314     C* A questo punto "estraggo" l'elemento corrente
035000080314     C                   IF        posA = 1
035100080314     C                   EVAL      SkSplitCSV(i) = *blanks
035200080314     C                   ELSE
035300080314     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
035400080314     C                                                    (posA-posDa))
035500080314     C                   ENDIF
035600080314     C                   ENDIF
035700080314     C*
035800080314     C* X gli elementi successivi al 1°
035900080314     C                   ELSE
036000080314     C*
036100080314     C* Parto a considerare l'elementoo corrente dal precedente separatore in poi
036200080314     C                   EVAL      posDa = posA + lenInSepara
036300080314     C*
036400080314     C* Verifico che nn vi sia il campo nn valorizzato
036500080314     C                   IF        %subst(InStringa:posDa:lenInSepara) =
036600080314     C                             %trim(InSepara)
036700080314     C*
036800080314     C* Se elemento nn valorizzato skippo al prossimo
036900080314     C                   EVAL      SkSplitCSV(i) = *blanks
037000080314     C                   EVAL      posA  = posA + 1
037100080314     C                   ELSE
037200080314     C*
037300080314     C* Eseguo lo scan x trovare la fine dell'elemento corrente
037400080314     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
037500080314     C                                     posDa+1)
037600080314     C*
037700080314     C* Gestisco l'ultimo campo
037800080314     C                   IF        posA = *zeros
037900080314     C                   EVAL      posA = %len(%trim(InStringa)) + 1
038000080314     C*
038100080314     C* A questo punto "estraggo" l'elemento corrente
038200080314     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
038300080314     C                                                    (posA-posDa))
038400080314     C                   LEAVE
038500080314     C                   ELSE
038600080314     C*
038700080314     C* A questo punto "estraggo" l'elemento corrente
038800080314     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
038900080314     C                                                    (posA-posDa))
039000080314     C                   ENDIF
039100080314     C                   ENDIF
039200080314     C                   ENDIF
039300080314     C*
039400080314     C* Incremento il contatore d campo
039500080314     C                   EVAL      i = i +1
039600080314     C                   ENDDO
039700080314     C*
039800080314     C                   ENDSR
039900010330
040000010330     C*----------------------------------------------------*
040100010330     C*  CONTROLLO NUMERICITA' CAMPI
040200010330     C*----------------------------------------------------*
040300010330     C     CHKNUM        BEGSR
040400010330     C*
040500010606     C                   IF        PiDecChr = *blanks
040600010606     C                   EVAL      PiDecChr = ','
040700010606     C                   ENDIF
040800010606     C*
040900010606     C                   CALL(e)   'ISNUMERIC'
041000010330     C                   PARM                    PiStr            30
041100010606     C                   PARM                    PiDecChr          1
041200010330     C                   PARM      *ZEROS        PiVal            30 9
041300010330     C                   PARM      '0'           PiInt             1
041400010330     C                   PARM      '0'           PiNum             1
041500010330     C                   IF        %error
041600010606     C                   EVAL      PiNum=*off
041700010330     C                   ENDIF
041800010330     C*
041900010330     C                   ENDSR
042000010330     C***
042100010601
042200060906     C*----------------------------------------------------*
042300060803      /TITLE Operazioni iniziali
042400060906     C*----------------------------------------------------*
042500010601     C     opeini        BEGSR
042600010601     C*
042700010601     C* Inizializzo flag e contatori operativi
042800010601     C                   movel     '0'           flgGiro           1
042900010615     C                   movel     '0'           flgOk             1
043000010601     C*
043100010601     C                   ENDSR
043200010601     C***
043300010330
043400060906     C*----------------------------------------------------*
043500000613     C     *inzsr        BEGSR
043600060906     C*----------------------------------------------------*
043700990910     C*
043800990910     C     *entry        plist
043900990920     C                   parm                    tivlrds
044000990921     C                   parm      wrkesito      esito
044100000724     C                   parm                    prmlit
044200000710     C                   parm                    prmfir
044300010330     C*
044400010330     C* CALCOLA LA DATA CORRENTE
044500120316     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
044600120316     C                   eval      datcor = %dec(%date() : *ISO)
044700000613     C*
044800000613     C                   ENDSR
