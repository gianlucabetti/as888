000100000000     H*------------------------------------------------------------------------*
000200090807     H* Statistica dirottamenti
000300091216     h* n.B.: originariamente nel file WFDIR venivano memorizzati anche colli
000400091216     h*       peso e volume. Poi I campi sono stati tolti dal file
000500091216     h*       ma nel pgm per il momento sono rimaste le memorizzazioni nelle schiere
000600091216     h*       anche se poi non più utilizzate
000700000000     H*------------------------------------------------------------------------*
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000000     F*------------------------------------------------------------------------*
001000000000     F* DATA BASE
001100000000     F*------------------------------------------------------------------------*
001200090825     FWFdir01L  UF A E           K DISK    usropn
001300000000     FTABEL00F  IF   E           K DISK
001400000000     FAZORG01L  IF   E           K DISK
001500091009     ftitas38c  if   e           k disk    extfile(wtas38) usropn
001600091203     ftitas30c  if   e           k disk    extfile(wtas30) usropn
001700091203     F                                     RENAME(titas000:titas30)
001800091203     F                                     RENAME(titas010:titas1030)
001900091203     F                                     RENAME(titasp00:titasp30)
002000090825     FQSYSPRT   O    F  132        PRINTER
002100091203     Fwfdirs0f  uf a e             disk    usropn
002200091203     ffnlbl02l  if   e           k disk
002300000000     L*------------------------------------------------------------------------*
002400090702     d ii              s              2  0
002500090825     d n               s              3  0
002600000000     D*------------------------------------------------------------------------*
002700000000     D* SCHIERE
002800000000     D*------------------------------------------------------------------------*
002900090825     D FIL             S              3  0 DIM(500) inz                         *CODICE FILIALE
003000090825     D DFIL            S             20    DIM(500) inz                         *DESCRIZIONE FILIALE
003100090825     d div             s              1    dim(500) inz
003200090825     d are             s              3  0 dim(500) inz
003300091103     d spe             s              8  0 dim(500) inz
003400091103     d cll             s              8  0 dim(500) inz
003500091103     d psk             s             11  1 dim(500) inz
003600091103     d vlm             s             11  3 dim(500) inz
003700091103     d spen            s              8  0 dim(500) inz
003800091103     d clln            s              8  0 dim(500) inz
003900091103     d pskn            s             11  1 dim(500) inz
004000091103     d vlmn            s             11  3 dim(500) inz
004100091216     d spea            s              8  0 dim(500) inz
004200090807     d s7o             s              1    dim(10)                              cca dirottamento
004300091103     d Tbo             s              2    dim(20)                              cca dirottamento
004400091216     d sop3m           s              1    dim(50)                              sigle oper.tab 3M
004500091106     d filfed          S              3  0 DIM(200)                             *filiali FED-EX
004600091106     d fildpd          S              3  0 DIM(200)                             *filiali DPD
004700091106     d wflag           s              1
004800090702      *
004900090702     d CTR             S              1    dim(10)
005000000000     D*------------------------------------------------------------------------*
005100000000     D* INPUT
005200000000     D*------------------------------------------------------------------------*
005300000706     D*-------------------
005400000706     D* LETTURA CAMPO ORGDE£ DI AZORG
005500000706     D*-------------------
005600000706     D OG143         E DS
005700000000     D*-------------------
005800000000     D* ARCHITETTURA
005900000000     D*-------------------
006000000000     D KPJBA         E DS
006100090807     D*-------------------
006200090807     d ds7o          E DS
006300091103     d dstb          E DS
006400090806     D*-------------------
006500090806     D* DS PARAMETRI DI LANCIO PGM
006600090806     D*-------------------
006700090806     D tist66ds      E DS
006800090807     d w_aasd                         4  0 overlay(st66dal)
006900090807     d w_mgsd                         4  0 overlay(st66dal:*next)
007000090807     d w_aasa                         4  0 overlay(st66al)
007100090826     d w_mgsa                         4  0 overlay(st66al:*next)
007200090824     D*-------------------
007300090824     D* DS PER TISI95R - CONTROLLO CAP
007400090824     D*-------------------
007500090824     d tisi95ds      E DS
007600090825     D*-------------------
007700090825     D tisit0ds      E DS
007800090826     d ddstflo       e ds
007900090807
008000090826     d codut           s              1  0 inz(1)
008100090807     d kcod            s                   like(tblcod)
008200090825     d kkey            s                   like(tblkey)
008300090826     d w_tk            s                   like(taspkf)
008400090826     d w_tm            s                   like(tasvlf)
008500090825     d o95lna_         s                   like(o95lna)
008600090825     d dataiso         s               d   datfmt(*iso)
008700090826     d datadmy         s               d   datfmt(*dmy)
008800090825     D datcor          S              8  0
008900090826     D data            S              6  0
009000090826     D ora             S              6  0
009100090825      * - Parametri per pgm. esecuzione comandi di sistema
009200090825     d Qcmd            s             80    inz
009300090825     d Qlen            s             15  5 inz(%len(Qcmd))
009400091009     d Wtas38          s             21    inz('GAITRAGRU /TITAS38C')
009500091203     d Wtas30          s             21    inz('GAITRAGRU /TITAS30C')
009600090825
009700090825      * - Comando per pulizia Work-File
009800090825     d C_Cmd_ClrWF     c                   const('CLRPFM FILE(*LIBL/+
009900090825     d                                                   WFDIR00F)')
010000091029     d C_Cmd_ClrWF1    c                   const('CLRPFM FILE(*LIBL/+
010100091029     d                                                   WFDIRS0F)')
010200000000     C*------------------------------------------------------------------------*
010300000000     C* MAIN LINE
010400000000     C*------------------------------------------------------------------------*
010500000000     C*
010600090805     c     keytas        setll     titas38c
010700090805     c                   read      titas38c
010800090807     c                   dow       not %eof(titas38c) and
010900090807     c                             (tasaas*10000+tasmgs)<=st66al
011000091119     c* memorizzo totale arrivato per lna scartando le bolle di recupero e c/s
011100091103     c                   if        %lookup(tastbl:tbo)=0
011200091103     c                   exsr      totarr
011300090807     c                   if        tascca<>*blanks and %lookup(tascca:s7o)>0
011400090807     c                   exsr      elabora
011500090807     c                   endif
011600091119     c                   endif
011700090805     c                   read      titas38c
011800090805     c                   enddo
011900091103     c* scarico totale arrivato dalle schiere al file
012000091103     c                   exsr      aggdir
012100091203     c* completo il file wfdirs con i dati della figlia
012200091203     c                   exsr      aggdirs
012300090825     C* OPERAZIONI FINALI
012400000000     C                   EXSR      FINSR
012500000000     C*
012600000000     C                   SETON                                        LR
012700091103     C*------------------------------------------------------------------------*
012800091103     C* Totarr  - Memorizza totale "arrivato" per linea di arrivo
012900091103     C*------------------------------------------------------------------------*
013000091103     C     Totarr        BEGSR
013100091103     c                   eval      n=%lookup(taslna:fil)
013200091103     c                   if        n>0
013300091103     c                   exsr      pesvol
013400091103     c                   if        tasnrs>0
013500091103     c                   add       1             spe(n)
013600091103     c                   add       tasncl        cll(n)
013700091103     c                   add       w_tk          psk(n)
013800091103     c                   add       w_tm          vlm(n)
013900091103     c                   else
014000091216     c                   if        %lookup(tassop:sop3m)>0
014100091216     c                   add       1             spea(n)
014200091216     c                   else
014300091103     c                   add       1             spen(n)
014400091103     c                   add       tasncl        clln(n)
014500091103     c                   add       w_tk          pskn(n)
014600091103     c                   add       w_tm          vlmn(n)
014700091103     c                   endif
014800091103     c                   endif
014900091216     c                   endif
015000091103     c                   endsr
015100090807     C*------------------------------------------------------------------------*
015200090807     C* Elabora - Elabora e memorizza i dati della spedizione letta
015300090807     C*------------------------------------------------------------------------*
015400090807     C     Elabora       BEGSR
015500090807     c
015600091106     c                   clear                   wflag
015700091030      *
015800091106    1c                   if        tasfgc<>'S' and tasfbr<>'S'
015900091030      *
016000090825     c* richiamo tisi95r per calcolare instradamento presunto spedizione
016100090824     c                   clear                   tisi95ds
016200090824     c                   movel     tasnzd        i95nar
016300090824     C                   MOVEL     tasCAD        I95CAP
016400090824     C                   MOVEL     tasLOD        I95LOC
016500090824     C                   MOVEL     tasPRD        I95PRV
016600090824     C                   MOVEL     tasFFD        I95FFD
016700090824     c                   z-ADD     TASPKB        I95LKG
016800091106    2C                   IF        TASNRS=0 OR tasfvb='R'
016900090824     c                   z-add     tasvlb        i95lmc
017000091106    2c                   endif
017100090824     c                   z-add     tastfp        i95tfp
017200090824     c                   movel     tastsp        i95tsp
017300090824     C                   MOVEL     '7'           I95TCN
017400090824     C                   eval      I95DAT=tasaas*10000+tasmgs
017500090824     C                   MOVEL     'S'           I95FRE
017600091106    2c                   select
017700091106     c                   when      %lookup(taslna:filfed)>0
017800090824     c                   movel     'S'           i95fi1
017900091106     c                   when      %lookup(taslna:fildpd)>0
018000091106     c                   movel     'D'           i95fi1
018100091106    2c                   endsl
018200090825     c                   movel     tastbl        i95tpo
018300090825     c* N.B.: Se c/a sulla seconda bolla in questo caso tasfca è vuoto
018400090825     c*       (unico e raro caso bolle '5'). Considerando poi che
018500090825     c*       i95fca non è nemmeno utilizzato per semplicità utilizzo
018600090825     c*       tasfca anzichè andare su tabella '3A'
018700090825     c                   movel     tasfca        i95fca
018800090826     c                   movel     tasftc        i95fi2
018900090824     c                   move      tastc2        i95fi2
019000090824     c                   call      'TISI95R'
019100090824     c                   parm                    tisi95ds
019200091030      *
019300091106    1c                   endif
019400090825     c*
019500090825     c* determina peso e volume da memorizzare
019600090825     c                   exsr      pesvol
019700090824
019800090824     c     taslna        chain     wfdir01l
019900090825     c                   if        not %found(wfdir01l)
020000090825     c                   clear                   wfdir000
020100090825     c                   endif
020200091106    1c                   if        tasnrs>0
020300090807     c* MEMORIZZAZIONI PER DISK "B"
020400090807     c* Totali
020500090824     c                   add       1             dirtsb
020600091106    2c                   select
020700091030     c* Di cui giacenti
020800091106     c                   when      tasfgc='S'
020900091030     c                   add       1             DIRDSBG
021000091106     c                   eval      wflag='G'
021100090807     c* Di cui con cap generico
021200091106     c                   when      o95err=*blanks and o95flg='S'
021300090824     c                   add       1             DIRDSBC
021400091106     c                   eval      wflag='C'
021500091106     c* Di cui con lna diversa da quella calcolata da tisi95 con consegna a
021600091106     c*    Supermercati
021700091106     c                   when      taslna<>o95lna and (tasftc='S' or tastc2='S')
021800091106     c                   add       1             DIRDSBs
021900091106     c                   eval      wflag='S'
022000090824     c* Di cui con lna diversa da quella calcolata da tisi95
022100091106     c                   when      taslna<>o95lna
022200090824     c                   add       1             DIRDSBa
022300091106     c                   eval      wflag='L'
022400091106    2c                   endsl
022500091106   x1c                   else
022600091216     c* MEMORIZZAZIONI PER ALTRI DISK
022700091217     c*   i resi vengono sempre memorizzati tra i no disk
022800091217     c
022900091217   1ac                   if        %lookup(tassop:sop3m)>0   and tasfbr<>'S'
023000091216     c* Totali
023100091216     c                   add       1             dirtsa
023200091216    2c                   select
023300091216     c* Di cui giacenti
023400091216     c                   when      tasfgc='S'
023500091216     c                   add       1             DIRDSaG
023600091216     c                   eval      wflag='G'
023700091216     c* Di cui bolle di reso
023800091217     c***                when      tasfbr='S'
023900091217     c***                add       1             DIRDSaR
024000091217     c***                eval      wflag='R'
024100091216     c* Di cui con cap generico
024200091216     c                   when      o95err=*blanks and o95flg='S'
024300091216     c                   add       1             DIRDSaC
024400091216     c                   eval      wflag='C'
024500091216     c                   other
024600091216     c* Di cui con lna diversa da quella calcolata da tisi95 e da eventuale
024700091216     c* lna forzata da anagrafica destinatari particolari
024800091216     c                   clear                   ddstflo
024900091216     c                   clear                   tisit0ds
025000091216     c                   movel     'E'           it0tla
025100091216     c                   movel     tasnzd        it0naz
025200091216     c                   movel     tasprd        it0prv
025300091216     c                   movel     tascad        it0cap
025400091216     c                   movel     taslod        it0loc
025500091216     c                   movel     tasind        it0ind
025600091216     c                   movel     tasrsd        it0rag
025700091216     c                   call      'TISIT5R'
025800091216     c                   parm                    tisit0ds
025900091216    3c     ot0err        ifeq      *blanks
026000091216     c                   movel     ot0flo        ddstflo
026100091216    3c                   endif
026200091216    3c                   if        §dstlna>*zeros
026300091216     c                   eval      o95lna_=%dec(§dstlna:3:0)
026400091216     c                   else
026500091216     c                   eval      o95lna_=o95lna
026600091216    3c                   endif
026700091216    3c                   if        taslna<>o95lna_
026800091216     c                   add       1             DIRDSaa
026900091216     c                   eval      wflag='L'
027000091216    3c                   endif
027100091216    2c                   endsl
027200091216  x1ac                   else
027300090825     c* MEMORIZZAZIONI PER NO DISK "B"
027400090825     c* Totali
027500090825     c                   add       1             dirtsn
027600091106    2c                   select
027700091030     c* Di cui giacenti
027800091106     c                   when      tasfgc='S'
027900091030     c                   add       1             DIRDSnG
028000091106     c                   eval      wflag='G'
028100091106     c* Di cui bolle di reso
028200091106     c                   when      tasfbr='S'
028300091106     c                   add       1             DIRDSnR
028400091106     c                   eval      wflag='R'
028500090825     c* Di cui con cap generico
028600091106     c                   when      o95err=*blanks and o95flg='S'
028700090825     c                   add       1             DIRDSnC
028800091106     c                   eval      wflag='C'
028900091106     c                   other
029000090825     c* Di cui con lna diversa da quella calcolata da tisi95 e da eventuale
029100090825     c* lna forzata da anagrafica destinatari particolari
029200090825     c                   clear                   ddstflo
029300090825     c                   clear                   tisit0ds
029400090825     c                   movel     'E'           it0tla
029500090825     c                   movel     tasnzd        it0naz
029600090825     c                   movel     tasprd        it0prv
029700090825     c                   movel     tascad        it0cap
029800090825     c                   movel     taslod        it0loc
029900090825     c                   movel     tasind        it0ind
030000090825     c                   movel     tasrsd        it0rag
030100090825     c                   call      'TISIT5R'
030200090825     c                   parm                    tisit0ds
030300091106    3c     ot0err        ifeq      *blanks
030400090825     c                   movel     ot0flo        ddstflo
030500091106    3c                   endif
030600091106    3c                   if        §dstlna>*zeros
030700090826     c                   eval      o95lna_=%dec(§dstlna:3:0)
030800090825     c                   else
030900090825     c                   eval      o95lna_=o95lna
031000091106    3c                   endif
031100091106    3c                   if        taslna<>o95lna_
031200090825     c                   add       1             DIRDSna
031300091106     c                   eval      wflag='L'
031400091106    3c                   endif
031500091106    2c                   endsl
031600091216   1ac                   endif
031700091106    1c                   endif
031800090825     c*
031900090825     c                   if        %found(wfdir01l)
032000090825     c                   update    wfdir000
032100090825     c                   else
032200090825     c                   exsr      rie_dir
032300090825     c                   write     wfdir000
032400090825     c                   endif
032500091106
032600091106     c* scrittura record dettaglio spedizione
032700091203     c                   clear                   wfdirs00
032800091106     c                   eval      dirsaas=tasaas
032900091106     c                   eval      DIRSLNP=taslnp
033000091106     c                   eval      DIRSNRS=tasnrs
033100091106     c                   eval      DIRSNSP=tasnsp
033200091106     c                   eval      DIRSLNA=taslna
033300091204     c                   eval      DIRSccm=tasccm
033400091106     c                   eval      dirsflg=wflag
033500091203     c                   eval      DIRSSOP=tassop
033600091203     c                   eval      DIRSRSD=tasrsd
033700091203     c                   eval      DIRSIND=tasind
033800091203     c                   eval      DIRSCAD=tascad
033900091203     c                   eval      DIRSLOD=taslod
034000091203     c                   eval      DIRSPRD=tasprd
034100091203     c                   eval      DIRSNZD=tasnzd
034200091203     c                   eval      DIRSNCL=tasncl
034300091203     c                   eval      DIRSPKB=taspkb
034400091203     c                   eval      DIRSVLB=tasvlb
034500091106     c                   write     wfdirs00
034600090825     c*
034700090807     c                   endsr
034800090807     C*------------------------------------------------------------------------*
034900090824     c* PESVOL - Determina il peso e volume da memorizzare
035000090807     C*------------------------------------------------------------------------*
035100090807     c     pesvol        begsr
035200090824     c*
035300090824     c                   clear                   w_tk
035400090824     c                   clear                   w_tm
035500090807     c* peso
035600090826    1c                   if        tasncp=tasncl or
035700090824     c                             taspkc>taspkf
035800090807     c                   z-add     taspkc        W_tk
035900090826   x1c                   else
036000090824     c                   z-add     taspkf        w_tk
036100090826    1c                   endif
036200090807     c* volume
036300090826    1c                   if        tasfvf='T'
036400090807     c                   z-add     tasvlf        W_tm
036500090826   x1c                   else
036600090826    2c                   if        tasncl=tasncr
036700090807     c                   z-add     tasvlc        w_tm
036800090826   x2c                   else
036900091008    3c                   if        tasvlc>tasvlf
037000090807     c                   z-add     tasvlc        W_tm
037100090826   x3c                   else
037200090807     c                   z-add     tasvlf        w_tm
037300090826    3c                   endif
037400090826    2c                   endif
037500090826    1c                   endif
037600090807     c                   endsr
037700091103     C*------------------------------------------------------------------------*
037800091103     C* RIE_DIR - Valorizzazione campi fissi del file di work
037900091103     C*------------------------------------------------------------------------*
038000091103     C     aggdir        BEGSR
038100091103     c     *loval        setll     wfdir01l
038200091103     c                   read      wfdir01l
038300091103     c                   dow       not %eof(wfdir01l)
038400091103     c                   eval      n=%lookup(dirlna:fil)
038500091103     c                   if        n>0
038600091103     c                   add       spe(n)        DIRTSBA
038700091103     c                   add       spen(n)       DIRTSNA
038800091216     c                   add       spea(n)       DIRTSAA
038900091103     c                   update    wfdir000
039000091103     c                   endif
039100091103     C
039200091103     c                   read      wfdir01l
039300091103     c                   enddo
039400091103     c                   endsr
039500091103     C*------------------------------------------------------------------------*
039600090825     C* RIE_DIR - Valorizzazione campi fissi del file di work
039700090825     C*------------------------------------------------------------------------*
039800090825     C     rie_dir       BEGSR
039900090825     c                   eval      DIRDEL=datcor
040000090825     C                   eval      DIRUSR=knmus
040100090825     c                   eval      DIRDAL=st66dal
040200090825     c                   eval      DIRALL=st66al
040300090825     c                   eval      n=%lookup(taslna:fil)
040400090825     c                   eval      DIRDIV=div(n)
040500090825     c                   exsr      decdiv
040600090825     c                   eval      DIRCAR=are(n)
040700090825     c                   exsr      decare
040800090825     c                   eval      DIRLNA=taslna
040900090825     c                   eval      DIRDLA=dfil(n)
041000090825     c                   endsr
041100091203     C*------------------------------------------------------------------------*
041200091203     C* aggdirs - aggiornamento file wfdirs con i dati della figlia
041300091203     C*------------------------------------------------------------------------*
041400091203     C     aggdirs       BEGSR
041500091203     c                   feod      wfdirs0f
041600091203     c     *start        setll     wfdirs0f
041700091203     c                   read      wfdirs0f
041800091203     c                   dow       not %eof(wfdirs0f)
041900091203     c     klbl          chain     fnlbl02l
042000091203     c                   if        %found(fnlbl02l)
042100091203     c     keytasf       chain     titas30c
042200091203     c                   if        %found(titas30c)
042300091203     c                   z-add     tasaas        DIRsAASf
042400091203     c                   z-add     taslnp        DIRsLNPf
042500091203     c                   z-add     tasnrs        DIRsNRSf
042600091203     c                   z-add     tasnsp        DIRsNSPf
042700091203     c                   z-add     tasmgs        DIRsMGSf
042800091203     c                   z-add     taslna        DIRslnaf
042900091203     c                   movel     tasrsd        DIRsRSDf
043000091203     c                   movel     tasind        DIRsINDf
043100091203     c                   movel     tascad        DIRsCADf
043200091203     c                   movel     taslod        DIRsLODf
043300091203     c                   movel     tasprd        DIRsPRDf
043400091203     c                   movel     tasnzd        DIRsNZDf
043500091203     c                   z-add     tasncl        DIRsNCLf
043600091203     c                   z-add     taspkb        DIRsPKBf
043700091203     c                   z-add     tasvlb        DIRsVLBf
043800091203     c                   update    wfdirs00
043900091203     c                   endif
044000091203     c                   endif
044100091203     c                   read      wfdirs0f
044200091203     c                   enddo
044300091203     c                   endsr
044400000000     C*------------------------------------------------------------------------*
044500000000     C* DECDIV - DECODIFICA DIVISIONE
044600000000     C*------------------------------------------------------------------------*
044700000000     C     DECDIV        BEGSR
044800000000     C*
044900090825     C                   MOVE      '17'          KCOD
045000090825     C                   MOVEL(p)  dirdiv        KKEY
045100090825     C     KEYTAB_C      CHAIN     TABEL
045200090825     C                   if        not %found(tabel00f) or tblflg='*'
045300090825     C                   MOVEL     *ALL'*'       Dirddi
045400000000     C                   ELSE
045500090825     C                   MOVEL     TBLUNI        dirddi
045600000000     C                   END
045700000000     C*
045800000000     C                   ENDSR
045900000000     C*------------------------------------------------------------------------*
046000000000     C* DECARE - DECODIFICA AREA
046100000000     C*------------------------------------------------------------------------*
046200000000     C     DECARE        BEGSR
046300000000     C*
046400090825     C                   MOVE      '05'          KCOD
046500090826     C                   MOVEL(P)  dircar        KKEY
046600090826     C     KEYTAB_c      CHAIN     TABEL
046700090825     C                   if        not %found(tabel00f) or tblflg='*'
046800090825     C                   MOVEL     *ALL'*'       Dirdar
046900000000     C                   ELSE
047000090825     C                   MOVEL     TBLUNI        Dirdar
047100000000     C                   END
047200000000     C*
047300000000     C                   ENDSR
047400000000     C*------------------------------------------------------------------------*
047500000000     C* FINSR - OPERAZIONI FINALI
047600000000     C*------------------------------------------------------------------------*
047700000000     C     FINSR         BEGSR
047800090825     c*
047900090825     C* generazione spool per indicare la fine elaborazione
048000090825     c                   except    finel
048100000000     C*
048200000000     C* CHIUDE I FILE DEI PGM CHIAMATI
048300090825     c                   clear                   tisit0ds
048400090825     c                   movel     'C'           it0tla
048500090825     C                   CALL      'TISIT5R'
048600090825     C                   PARM                    tisit0ds
048700090825     c*
048800090825     C                   CLEAR                   tisi95ds
048900090825     C                   MOVEL     'C'           I95TLA
049000090825     C                   CALL      'TISI95R'
049100090825     C                   PARM                    tisi95ds
049200000000     C*
049300000000     C                   ENDSR
049400000000     C*------------------------------------------------------------------------*
049500000000     C* *INZSR - ROUTINE INIZIALE
049600000000     C*------------------------------------------------------------------------*
049700000000     C     *INZSR        BEGSR
049800000000     C*--------------------
049900090806     C* RICEVIMENTO PARAMETRI
050000000000     C*--------------------
050100000000     C     *ENTRY        PLIST
050200000000     C                   PARM                    KPJBA
050300090806     C                   MOVEL     KPJBU         tist66ds
050400000000     C*
050500000000     C*--------------------
050600000000     C* CHIAVI DI LETTURA
050700000000     C*--------------------
050800090807     c     keytas        klist
050900090807     c                   kfld                    w_aasd
051000090807     c                   kfld                    w_mgsd
051100090807     c*
051200091203     c     keytasf       klist
051300091203     c                   kfld                    LBLAAN
051400091203     c                   kfld                    LBLLPN
051500091203     c                   kfld                    LBLNRN
051600091203     c                   kfld                    LBLNSN
051700091203     c*
051800091203     c     klbl          klist
051900091203     c                   kfld                    dirsaas
052000091203     c                   kfld                    dirslnp
052100091203     c                   kfld                    dirsnrs
052200091203     c                   kfld                    dirsnsp
052300090807     C* CHIAVE LETTURA TABEL00F
052400000000     C     KEYTAB        KLIST
052500090807     C                   KFLD                    codut
052600090807     C                   KFLD                    KCOD
052700090825     C     KEYTAB_C      KLIST
052800090825     C                   KFLD                    codut
052900090825     C                   KFLD                    KCOD
053000090825     C                   KFLD                    Kkey
053100090807     c*
053200090807     c* Caricamento schiera causali consegna anomala di dirottamento
053300090807     c                   clear                   ii
053400090807     c                   eval      kcod='7O'
053500090826     c     keytab        setll     TABEL00F
053600090826     c     keytab        reade     TABEL00F
053700090826     c                   dow       not %eof(TABEL00F)
053800090807     c                   movel     tbluni        ds7o
053900090807     c                   if        §7ofdi='D'
054000090807     c                   add       1             ii
054100090807     c                   movel     tblkey        s7o(ii)
054200090807     c                   endif
054300090826     c     keytab        reade     TABEL00F
054400090807     c                   enddo
054500091119     c* Caricamento schiera tipi bolla di recupero  e c/s
054600091103     C                   clear                   ii
054700091103     c                   eval      kcod='TB'
054800091103     C     keytab        SETLL     TABEL00F
054900091103     C     keytab        reade     TABEL00F
055000091103     c                   dow       not %eof(tabel00f)
055100091103     C     TBLFLG        IFNE      '*'
055200091103     C                   MOVEL     TBLUNI        DSTB
055300091103     C     §TBRBL        IFEQ      'R'
055400091119     C     §TBRBL        oreQ      'C'
055500091103     C                   ADD       1             ii
055600091103     C                   MOVEL     TBLKEY        TBO(ii)
055700091103     C                   ENDIF
055800091103     C                   ENDIF
055900091103     C     keytab        reade     TABEL00F
056000091103     C                   ENDDO
056100091216     c* Caricamento schiera sigle operatore tabella "3M"
056200091216     C                   clear                   ii
056300091216     c                   eval      kcod='3M'
056400091216     C     keytab        SETLL     TABEL00F
056500091216     C     keytab        reade     TABEL00F
056600091216     c                   dow       not %eof(tabel00f)
056700091216     C                   ADD       1             ii
056800091216     C                   MOVEL     TBLKEY        sop3m(ii)
056900091216     C     keytab        reade     TABEL00F
057000091216     C                   ENDDO
057100000000     C*
057200020910     C*--------------------
057300020910     C* REPERISCE DATA/ORA CORRENTE
057400020910     C*--------------------
057500090825     C                   TIME                    dataiso
057600090825     c                   move      dataiso       datcor
057700090826     c                   move      dataiso       datadmy
057800090826     c                   move      datadmy       data
057900090826     c                   TIME                    ora
058000000000     C* CARICA ORGANIGRAMMA
058100000000     C                   EXSR      CARORG
058200090825     c*
058300090825     c* Pulizia file wfdir prima di scriverlo
058400090825     c                   call      'QCMDEXC'
058500090825     c                   parm      C_Cmd_ClrWF   Qcmd
058600090825     c                   parm                    Qlen
058700090825     c
058800090825     c                   open      WFDIR01L
058900091029
059000091029     c* Pulizia file wfdirs prima di scriverlo
059100091029     c                   call      'QCMDEXC'
059200091029     c                   parm      C_Cmd_ClrWF1  Qcmd
059300091029     c                   parm                    Qlen
059400091029     c
059500091029     c                   open      WFDIRS0F
059600091009     c*
059700091009     c                   open(E)   titas38c
059800091009     c                   if        not %open(titas38c)
059900091009     c                   eval      %subst(wtas38:7:4)='GRPS'
060000091009     c                   open      titas38c
060100091009     c                   endif
060200091203      *
060300091203     c                   open(E)   titas30c
060400091203     c                   if        not %open(titas30c)
060500091203     c                   eval      %subst(wtas30:7:4)='GRPS'
060600091203     c                   open      titas30c
060700091203     c                   endif
060800000000     C*
060900000000     C                   ENDSR
061000000000     C*------------------------------------------------------------------------*
061100000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
061200000000     C*------------------------------------------------------------------------*
061300000000     C     CARORG        BEGSR
061400000000     C*
061500000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
061600090825     C     *loval        SETLL     AZORG01L
061700090825     C                   READ      AZORG01L
061800090825     C                   Z-ADD     0             I                 3 0
061900090825     C                   Z-ADD     0             k                 3 0
062000091106     C                   Z-ADD     0             y                 3 0
062100090825    1C                   DOW       not %eof(azorg01l)
062200090825    2C     ORGFAG        IFNE      'V'
062300000000     C                   ADD       1             I
062400000000     C                   Z-ADD     ORGFIL        FIL(I)
062500000000     C                   MOVEL     ORGDES        DFIL(I)
062600090825     c                   movel     orgfl3        div(I)
062700090825     c                   z-add     orgcar        are(I)
062800040308     C                   MOVEL     ORGDE3        OG143
062900090825    3c                   if        §ogntw='FED'
063000090824     c                   ADD       1             K
063100090824     c                   Z-ADD     orgfil        filfed(k)
063200090825    3c                   endif
063300091106    3c                   if        §ogntw='DPD'
063400091106     c                   ADD       1             y
063500091106     c                   Z-ADD     orgfil        fildpd(y)
063600091106    3c                   endif
063700090825    2C                   END
063800090825     C                   READ      AZORG01L
063900090825    1C                   ENDdo
064000000000     C*
064100000000     C                   ENDSR
064200000000     O*------------------------------------------------------------------------*
064300090825     Oqsysprt   E            finel          1 02
064400090825     O                                            8 'TIST66R '
064500090826     O                                           46 '***      STATISTICA  '
064600090826     O                                           67 'DIROTTAMENTI    **** '
064700090826     O                       DATA               110 '  /  /  '
064800090826     O                       ORA                119 '  :  :  '
064900090826     O                                          124 'Pag.'
065000090826     O                       PAGE          Z    128
065100000000     O*------------------------
065200090825     O          E            finel          3
065300090825     O                                          110 '*** FINE ELABORAZIONE ***'
