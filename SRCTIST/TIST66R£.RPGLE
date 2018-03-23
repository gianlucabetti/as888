000100000000     H*------------------------------------------------------------------------*
000200090807     H* Statistica dirottamenti
000201091216     h* n.B.: originariamente nel file WFDIR venivano memorizzati anche colli
000202091216     h*       peso e volume. Poi I campi sono stati tolti dal file
000203091216     h*       ma nel pgm per il momento sono rimaste le memorizzazioni nelle schiere
000204091216     h*       anche se poi non più utilizzate
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800090825     FWFdir01L  UF A E           K DISK    usropn
000900000000     FTABEL00F  IF   E           K DISK
001000000000     FAZORG01L  IF   E           K DISK
001100091009     ftitas38c  if   e           k disk    extfile(wtas38) usropn
001101091203     ftitas30c  if   e           k disk    extfile(wtas30) usropn
001102091203     F                                     RENAME(titas000:titas30)
001103091203     F                                     RENAME(titas010:titas1030)
001104091203     F                                     RENAME(titasp00:titasp30)
001105090825     FQSYSPRT   O    F  132        PRINTER
001106091203     Fwfdirs0f  uf a e             disk    usropn
001107091203     ffnlbl02l  if   e           k disk
001500000000     L*------------------------------------------------------------------------*
001602090702     d ii              s              2  0
001603090825     d n               s              3  0
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002900090825     D FIL             S              3  0 DIM(500) inz                         *CODICE FILIALE
003000090825     D DFIL            S             20    DIM(500) inz                         *DESCRIZIONE FILIALE
003001090825     d div             s              1    dim(500) inz
003002090825     d are             s              3  0 dim(500) inz
003003091103     d spe             s              8  0 dim(500) inz
003004091103     d cll             s              8  0 dim(500) inz
003005091103     d psk             s             11  1 dim(500) inz
003006091103     d vlm             s             11  3 dim(500) inz
003007091103     d spen            s              8  0 dim(500) inz
003008091103     d clln            s              8  0 dim(500) inz
003009091103     d pskn            s             11  1 dim(500) inz
003010091103     d vlmn            s             11  3 dim(500) inz
003011091216     d spea            s              8  0 dim(500) inz
003101090807     d s7o             s              1    dim(10)                              cca dirottamento
003102091103     d Tbo             s              2    dim(20)                              cca dirottamento
003103091216     d sop3m           s              1    dim(50)                              sigle oper.tab 3M
003104091106     d filfed          S              3  0 DIM(200)                             *filiali FED-EX
003105091106     d fildpd          S              3  0 DIM(200)                             *filiali DPD
003106091106     d wflag           s              1
003107090702      *
003108090702     d CTR             S              1    dim(10)
003200000000     D*------------------------------------------------------------------------*
003300000000     D* INPUT
003400000000     D*------------------------------------------------------------------------*
003900000706     D*-------------------
004000000706     D* LETTURA CAMPO ORGDE£ DI AZORG
004100000706     D*-------------------
004200000706     D OG143         E DS
004300000000     D*-------------------
004400000000     D* ARCHITETTURA
004500000000     D*-------------------
004600000000     D KPJBA         E DS
004601090807     D*-------------------
004603090807     d ds7o          E DS
004604091103     d dstb          E DS
004605090806     D*-------------------
004606090806     D* DS PARAMETRI DI LANCIO PGM
004607090806     D*-------------------
004608090806     D tist66ds      E DS
004609090807     d w_aasd                         4  0 overlay(st66dal)
004610090807     d w_mgsd                         4  0 overlay(st66dal:*next)
004611090807     d w_aasa                         4  0 overlay(st66al)
004612090826     d w_mgsa                         4  0 overlay(st66al:*next)
004613090824     D*-------------------
004614090824     D* DS PER TISI95R - CONTROLLO CAP
004615090824     D*-------------------
004616090824     d tisi95ds      E DS
004617090825     D*-------------------
004618090825     D tisit0ds      E DS
004619090826     d ddstflo       e ds
007501090807
007502090826     d codut           s              1  0 inz(1)
007503090807     d kcod            s                   like(tblcod)
007504090825     d kkey            s                   like(tblkey)
007505090826     d w_tk            s                   like(taspkf)
007506090826     d w_tm            s                   like(tasvlf)
007507090825     d o95lna_         s                   like(o95lna)
007508090825     d dataiso         s               d   datfmt(*iso)
007509090826     d datadmy         s               d   datfmt(*dmy)
007510090825     D datcor          S              8  0
007511090826     D data            S              6  0
007512090826     D ora             S              6  0
007518090825      * - Parametri per pgm. esecuzione comandi di sistema
007519090825     d Qcmd            s             80    inz
007520090825     d Qlen            s             15  5 inz(%len(Qcmd))
007521091009     d Wtas38          s             21    inz('GAITRAGRU /TITAS38C')
007522091203     d Wtas30          s             21    inz('GAITRAGRU /TITAS30C')
007523090825
007524090825      * - Comando per pulizia Work-File
007525090825     d C_Cmd_ClrWF     c                   const('CLRPFM FILE(*LIBL/+
007526090825     d                                                   WFDIR00F)')
007527091029     d C_Cmd_ClrWF1    c                   const('CLRPFM FILE(*LIBL/+
007528091029     d                                                   WFDIRS0F)')
010000000000     C*------------------------------------------------------------------------*
010100000000     C* MAIN LINE
010200000000     C*------------------------------------------------------------------------*
010300000000     C*
010301090805     c     keytas        setll     titas38c
010302090805     c                   read      titas38c
010303090807     c                   dow       not %eof(titas38c) and
010304090807     c                             (tasaas*10000+tasmgs)<=st66al
010305091119     c* memorizzo totale arrivato per lna scartando le bolle di recupero e c/s
010306091103     c                   if        %lookup(tastbl:tbo)=0
010307091103     c                   exsr      totarr
010309090807     c                   if        tascca<>*blanks and %lookup(tascca:s7o)>0
010310090807     c                   exsr      elabora
010311090807     c                   endif
010312091119     c                   endif
010313090805     c                   read      titas38c
010314090805     c                   enddo
010315091103     c* scarico totale arrivato dalle schiere al file
010316091103     c                   exsr      aggdir
010317091203     c* completo il file wfdirs con i dati della figlia
010318091203     c                   exsr      aggdirs
020200090825     C* OPERAZIONI FINALI
020400000000     C                   EXSR      FINSR
020500000000     C*
020600000000     C                   SETON                                        LR
020601091103     C*------------------------------------------------------------------------*
020602091103     C* Totarr  - Memorizza totale "arrivato" per linea di arrivo
020603091103     C*------------------------------------------------------------------------*
020604091103     C     Totarr        BEGSR
020605091103     c                   eval      n=%lookup(taslna:fil)
020606091103     c                   if        n>0
020607091103     c                   exsr      pesvol
020608091103     c                   if        tasnrs>0
020609091103     c                   add       1             spe(n)
020610091103     c                   add       tasncl        cll(n)
020611091103     c                   add       w_tk          psk(n)
020612091103     c                   add       w_tm          vlm(n)
020613091103     c                   else
020614091216     c                   if        %lookup(tassop:sop3m)>0
020615091216     c                   add       1             spea(n)
020616091216     c                   else
020619091103     c                   add       1             spen(n)
020620091103     c                   add       tasncl        clln(n)
020621091103     c                   add       w_tk          pskn(n)
020622091103     c                   add       w_tm          vlmn(n)
020623091103     c                   endif
020624091103     c                   endif
020625091216     c                   endif
020626091103     c                   endsr
020627090807     C*------------------------------------------------------------------------*
020628090807     C* Elabora - Elabora e memorizza i dati della spedizione letta
020629090807     C*------------------------------------------------------------------------*
020630090807     C     Elabora       BEGSR
020631090807     c
020632091106     c                   clear                   wflag
020633091030      *
020634091106    1c                   if        tasfgc<>'S' and tasfbr<>'S'
020635091030      *
020636090825     c* richiamo tisi95r per calcolare instradamento presunto spedizione
020637090824     c                   clear                   tisi95ds
020638090824     c                   movel     tasnzd        i95nar
020639090824     C                   MOVEL     tasCAD        I95CAP
020640090824     C                   MOVEL     tasLOD        I95LOC
020641090824     C                   MOVEL     tasPRD        I95PRV
020642090824     C                   MOVEL     tasFFD        I95FFD
020643090824     c                   z-ADD     TASPKB        I95LKG
020644091106    2C                   IF        TASNRS=0 OR tasfvb='R'
020645090824     c                   z-add     tasvlb        i95lmc
020646091106    2c                   endif
020647090824     c                   z-add     tastfp        i95tfp
020648090824     c                   movel     tastsp        i95tsp
020649090824     C                   MOVEL     '7'           I95TCN
020650090824     C                   eval      I95DAT=tasaas*10000+tasmgs
020651090824     C                   MOVEL     'S'           I95FRE
020652091106    2c                   select
020653091106     c                   when      %lookup(taslna:filfed)>0
020654090824     c                   movel     'S'           i95fi1
020655091106     c                   when      %lookup(taslna:fildpd)>0
020656091106     c                   movel     'D'           i95fi1
020657091106    2c                   endsl
020658090825     c                   movel     tastbl        i95tpo
020659090825     c* N.B.: Se c/a sulla seconda bolla in questo caso tasfca è vuoto
020660090825     c*       (unico e raro caso bolle '5'). Considerando poi che
020661090825     c*       i95fca non è nemmeno utilizzato per semplicità utilizzo
020662090825     c*       tasfca anzichè andare su tabella '3A'
020663090825     c                   movel     tasfca        i95fca
020664090826     c                   movel     tasftc        i95fi2
020665090824     c                   move      tastc2        i95fi2
020666090824     c                   call      'TISI95R'
020667090824     c                   parm                    tisi95ds
020668091030      *
020669091106    1c                   endif
020670090825     c*
020671090825     c* determina peso e volume da memorizzare
020672090825     c                   exsr      pesvol
020673090824
020674090824     c     taslna        chain     wfdir01l
020675090825     c                   if        not %found(wfdir01l)
020676090825     c                   clear                   wfdir000
020677090825     c                   endif
020678091106    1c                   if        tasnrs>0
020679090807     c* MEMORIZZAZIONI PER DISK "B"
020680090807     c* Totali
020681090824     c                   add       1             dirtsb
020685091106    2c                   select
020686091030     c* Di cui giacenti
020687091106     c                   when      tasfgc='S'
020688091030     c                   add       1             DIRDSBG
020692091106     c                   eval      wflag='G'
020700090807     c* Di cui con cap generico
020701091106     c                   when      o95err=*blanks and o95flg='S'
020702090824     c                   add       1             DIRDSBC
020706091106     c                   eval      wflag='C'
020707091106     c* Di cui con lna diversa da quella calcolata da tisi95 con consegna a
020708091106     c*    Supermercati
020709091106     c                   when      taslna<>o95lna and (tasftc='S' or tastc2='S')
020710091106     c                   add       1             DIRDSBs
020714091106     c                   eval      wflag='S'
020715090824     c* Di cui con lna diversa da quella calcolata da tisi95
020716091106     c                   when      taslna<>o95lna
020717090824     c                   add       1             DIRDSBa
020721091106     c                   eval      wflag='L'
020722091106    2c                   endsl
020724091106   x1c                   else
020725091216     c* MEMORIZZAZIONI PER ALTRI DISK
020726091216   1ac                   if        %lookup(tassop:sop3m)>0
020727091216     c* Totali
020728091216     c                   add       1             dirtsa
020729091216    2c                   select
020730091216     c* Di cui giacenti
020731091216     c                   when      tasfgc='S'
020732091216     c                   add       1             DIRDSaG
020736091216     c                   eval      wflag='G'
020737091216     c* Di cui bolle di reso
020738091216     c                   when      tasfbr='S'
020739091216     c                   add       1             DIRDSaR
020743091216     c                   eval      wflag='R'
020744091216     c* Di cui con cap generico
020745091216     c                   when      o95err=*blanks and o95flg='S'
020746091216     c                   add       1             DIRDSaC
020750091216     c                   eval      wflag='C'
020751091216     c                   other
020752091216     c* Di cui con lna diversa da quella calcolata da tisi95 e da eventuale
020753091216     c* lna forzata da anagrafica destinatari particolari
020754091216     c                   clear                   ddstflo
020755091216     c                   clear                   tisit0ds
020756091216     c                   movel     'E'           it0tla
020757091216     c                   movel     tasnzd        it0naz
020758091216     c                   movel     tasprd        it0prv
020759091216     c                   movel     tascad        it0cap
020760091216     c                   movel     taslod        it0loc
020761091216     c                   movel     tasind        it0ind
020762091216     c                   movel     tasrsd        it0rag
020763091216     c                   call      'TISIT5R'
020764091216     c                   parm                    tisit0ds
020765091216    3c     ot0err        ifeq      *blanks
020766091216     c                   movel     ot0flo        ddstflo
020767091216    3c                   endif
020768091216    3c                   if        §dstlna>*zeros
020769091216     c                   eval      o95lna_=%dec(§dstlna:3:0)
020770091216     c                   else
020771091216     c                   eval      o95lna_=o95lna
020772091216    3c                   endif
020773091216    3c                   if        taslna<>o95lna_
020774091216     c                   add       1             DIRDSaa
020778091216     c                   eval      wflag='L'
020779091216    3c                   endif
020780091216    2c                   endsl
020781091216  x1ac                   else
020782090825     c* MEMORIZZAZIONI PER NO DISK "B"
020783090825     c* Totali
020784090825     c                   add       1             dirtsn
020788091106    2c                   select
020789091030     c* Di cui giacenti
020790091106     c                   when      tasfgc='S'
020791091030     c                   add       1             DIRDSnG
020795091106     c                   eval      wflag='G'
020796091106     c* Di cui bolle di reso
020797091106     c                   when      tasfbr='S'
020798091106     c                   add       1             DIRDSnR
020802091106     c                   eval      wflag='R'
020803090825     c* Di cui con cap generico
020804091106     c                   when      o95err=*blanks and o95flg='S'
020805090825     c                   add       1             DIRDSnC
020809091106     c                   eval      wflag='C'
020810091106     c                   other
020811090825     c* Di cui con lna diversa da quella calcolata da tisi95 e da eventuale
020812090825     c* lna forzata da anagrafica destinatari particolari
020813090825     c                   clear                   ddstflo
020814090825     c                   clear                   tisit0ds
020815090825     c                   movel     'E'           it0tla
020816090825     c                   movel     tasnzd        it0naz
020817090825     c                   movel     tasprd        it0prv
020818090825     c                   movel     tascad        it0cap
020819090825     c                   movel     taslod        it0loc
020820090825     c                   movel     tasind        it0ind
020821090825     c                   movel     tasrsd        it0rag
020822090825     c                   call      'TISIT5R'
020823090825     c                   parm                    tisit0ds
020824091106    3c     ot0err        ifeq      *blanks
020825090825     c                   movel     ot0flo        ddstflo
020826091106    3c                   endif
020827091106    3c                   if        §dstlna>*zeros
020828090826     c                   eval      o95lna_=%dec(§dstlna:3:0)
020829090825     c                   else
020830090825     c                   eval      o95lna_=o95lna
020831091106    3c                   endif
020832091106    3c                   if        taslna<>o95lna_
020833090825     c                   add       1             DIRDSna
020837091106     c                   eval      wflag='L'
020838091106    3c                   endif
020839091106    2c                   endsl
020840091216   1ac                   endif
020841091106    1c                   endif
020842090825     c*
020843090825     c                   if        %found(wfdir01l)
020844090825     c                   update    wfdir000
020845090825     c                   else
020846090825     c                   exsr      rie_dir
020847090825     c                   write     wfdir000
020848090825     c                   endif
020849091106
020850091106     c* scrittura record dettaglio spedizione
020851091203     c                   clear                   wfdirs00
020852091106     c                   eval      dirsaas=tasaas
020853091106     c                   eval      DIRSLNP=taslnp
020854091106     c                   eval      DIRSNRS=tasnrs
020855091106     c                   eval      DIRSNSP=tasnsp
020856091106     c                   eval      DIRSLNA=taslna
020857091204     c                   eval      DIRSccm=tasccm
020858091106     c                   eval      dirsflg=wflag
020859091203     c                   eval      DIRSSOP=tassop
020860091203     c                   eval      DIRSRSD=tasrsd
020861091203     c                   eval      DIRSIND=tasind
020862091203     c                   eval      DIRSCAD=tascad
020863091203     c                   eval      DIRSLOD=taslod
020864091203     c                   eval      DIRSPRD=tasprd
020865091203     c                   eval      DIRSNZD=tasnzd
020866091203     c                   eval      DIRSNCL=tasncl
020867091203     c                   eval      DIRSPKB=taspkb
020868091203     c                   eval      DIRSVLB=tasvlb
020869091106     c                   write     wfdirs00
020870090825     c*
020871090807     c                   endsr
020872090807     C*------------------------------------------------------------------------*
020873090824     c* PESVOL - Determina il peso e volume da memorizzare
020874090807     C*------------------------------------------------------------------------*
020875090807     c     pesvol        begsr
020876090824     c*
020877090824     c                   clear                   w_tk
020878090824     c                   clear                   w_tm
020879090807     c* peso
020880090826    1c                   if        tasncp=tasncl or
020881090824     c                             taspkc>taspkf
020882090807     c                   z-add     taspkc        W_tk
020883090826   x1c                   else
020884090824     c                   z-add     taspkf        w_tk
020885090826    1c                   endif
020886090807     c* volume
020887090826    1c                   if        tasfvf='T'
020888090807     c                   z-add     tasvlf        W_tm
020889090826   x1c                   else
020890090826    2c                   if        tasncl=tasncr
020891090807     c                   z-add     tasvlc        w_tm
020892090826   x2c                   else
020893091008    3c                   if        tasvlc>tasvlf
020894090807     c                   z-add     tasvlc        W_tm
020895090826   x3c                   else
020896090807     c                   z-add     tasvlf        w_tm
020897090826    3c                   endif
020898090826    2c                   endif
020899090826    1c                   endif
020900090807     c                   endsr
020901091103     C*------------------------------------------------------------------------*
020902091103     C* RIE_DIR - Valorizzazione campi fissi del file di work
020903091103     C*------------------------------------------------------------------------*
020904091103     C     aggdir        BEGSR
020905091103     c     *loval        setll     wfdir01l
020906091103     c                   read      wfdir01l
020907091103     c                   dow       not %eof(wfdir01l)
020908091103     c                   eval      n=%lookup(dirlna:fil)
020909091103     c                   if        n>0
020910091103     c                   add       spe(n)        DIRTSBA
020914091103     c                   add       spen(n)       DIRTSNA
020918091216     c                   add       spea(n)       DIRTSAA
020919091103     c                   update    wfdir000
020920091103     c                   endif
020921091103     C
020922091103     c                   read      wfdir01l
020923091103     c                   enddo
020924091103     c                   endsr
020925091103     C*------------------------------------------------------------------------*
020926090825     C* RIE_DIR - Valorizzazione campi fissi del file di work
020927090825     C*------------------------------------------------------------------------*
020928090825     C     rie_dir       BEGSR
020929090825     c                   eval      DIRDEL=datcor
020930090825     C                   eval      DIRUSR=knmus
020931090825     c                   eval      DIRDAL=st66dal
020932090825     c                   eval      DIRALL=st66al
020933090825     c                   eval      n=%lookup(taslna:fil)
020934090825     c                   eval      DIRDIV=div(n)
020935090825     c                   exsr      decdiv
020936090825     c                   eval      DIRCAR=are(n)
020937090825     c                   exsr      decare
020938090825     c                   eval      DIRLNA=taslna
020939090825     c                   eval      DIRDLA=dfil(n)
020940090825     c                   endsr
020941091203     C*------------------------------------------------------------------------*
020942091203     C* aggdirs - aggiornamento file wfdirs con i dati della figlia
020943091203     C*------------------------------------------------------------------------*
020944091203     C     aggdirs       BEGSR
020945091203     c                   feod      wfdirs0f
020946091203     c     *start        setll     wfdirs0f
020947091203     c                   read      wfdirs0f
020948091203     c                   dow       not %eof(wfdirs0f)
020949091203     c     klbl          chain     fnlbl02l
020950091203     c                   if        %found(fnlbl02l)
020951091203     c     keytasf       chain     titas30c
020952091203     c                   if        %found(titas30c)
020953091203     c                   z-add     tasaas        DIRsAASf
020954091203     c                   z-add     taslnp        DIRsLNPf
020955091203     c                   z-add     tasnrs        DIRsNRSf
020956091203     c                   z-add     tasnsp        DIRsNSPf
020957091203     c                   z-add     tasmgs        DIRsMGSf
020958091203     c                   z-add     taslna        DIRslnaf
020959091203     c                   movel     tasrsd        DIRsRSDf
020960091203     c                   movel     tasind        DIRsINDf
020961091203     c                   movel     tascad        DIRsCADf
020962091203     c                   movel     taslod        DIRsLODf
020963091203     c                   movel     tasprd        DIRsPRDf
020964091203     c                   movel     tasnzd        DIRsNZDf
020965091203     c                   z-add     tasncl        DIRsNCLf
020966091203     c                   z-add     taspkb        DIRsPKBf
020967091203     c                   z-add     tasvlb        DIRsVLBf
020968091203     c                   update    wfdirs00
020969091203     c                   endif
020970091203     c                   endif
020971091203     c                   read      wfdirs0f
020972091203     c                   enddo
020973091203     c                   endsr
100000000000     C*------------------------------------------------------------------------*
100100000000     C* DECDIV - DECODIFICA DIVISIONE
100200000000     C*------------------------------------------------------------------------*
100300000000     C     DECDIV        BEGSR
100400000000     C*
100600090825     C                   MOVE      '17'          KCOD
100800090825     C                   MOVEL(p)  dirdiv        KKEY
100900090825     C     KEYTAB_C      CHAIN     TABEL
101000090825     C                   if        not %found(tabel00f) or tblflg='*'
101200090825     C                   MOVEL     *ALL'*'       Dirddi
101300000000     C                   ELSE
101400090825     C                   MOVEL     TBLUNI        dirddi
101500000000     C                   END
101600000000     C*
101700000000     C                   ENDSR
101800000000     C*------------------------------------------------------------------------*
101900000000     C* DECARE - DECODIFICA AREA
102000000000     C*------------------------------------------------------------------------*
102100000000     C     DECARE        BEGSR
102200000000     C*
102400090825     C                   MOVE      '05'          KCOD
102600090826     C                   MOVEL(P)  dircar        KKEY
102700090826     C     KEYTAB_c      CHAIN     TABEL
102701090825     C                   if        not %found(tabel00f) or tblflg='*'
103000090825     C                   MOVEL     *ALL'*'       Dirdar
103100000000     C                   ELSE
103200090825     C                   MOVEL     TBLUNI        Dirdar
103300000000     C                   END
103400000000     C*
103500000000     C                   ENDSR
118200000000     C*------------------------------------------------------------------------*
118300000000     C* FINSR - OPERAZIONI FINALI
118400000000     C*------------------------------------------------------------------------*
118500000000     C     FINSR         BEGSR
118501090825     c*
118502090825     C* generazione spool per indicare la fine elaborazione
118503090825     c                   except    finel
118600000000     C*
118700000000     C* CHIUDE I FILE DEI PGM CHIAMATI
118701090825     c                   clear                   tisit0ds
118702090825     c                   movel     'C'           it0tla
118703090825     C                   CALL      'TISIT5R'
118704090825     C                   PARM                    tisit0ds
118705090825     c*
118706090825     C                   CLEAR                   tisi95ds
118707090825     C                   MOVEL     'C'           I95TLA
118708090825     C                   CALL      'TISI95R'
118709090825     C                   PARM                    tisi95ds
120100000000     C*
120200000000     C                   ENDSR
120300000000     C*------------------------------------------------------------------------*
120400000000     C* *INZSR - ROUTINE INIZIALE
120500000000     C*------------------------------------------------------------------------*
120600000000     C     *INZSR        BEGSR
120700000000     C*--------------------
120800090806     C* RICEVIMENTO PARAMETRI
120900000000     C*--------------------
121000000000     C     *ENTRY        PLIST
121100000000     C                   PARM                    KPJBA
121200090806     C                   MOVEL     KPJBU         tist66ds
123500000000     C*
123600000000     C*--------------------
123700000000     C* CHIAVI DI LETTURA
123800000000     C*--------------------
123801090807     c     keytas        klist
123802090807     c                   kfld                    w_aasd
123803090807     c                   kfld                    w_mgsd
123804090807     c*
123805091203     c     keytasf       klist
123806091203     c                   kfld                    LBLAAN
123807091203     c                   kfld                    LBLLPN
123808091203     c                   kfld                    LBLNRN
123809091203     c                   kfld                    LBLNSN
123810091203     c*
123811091203     c     klbl          klist
123812091203     c                   kfld                    dirsaas
123813091203     c                   kfld                    dirslnp
123814091203     c                   kfld                    dirsnrs
123815091203     c                   kfld                    dirsnsp
123900090807     C* CHIAVE LETTURA TABEL00F
124000000000     C     KEYTAB        KLIST
124100090807     C                   KFLD                    codut
124200090807     C                   KFLD                    KCOD
124201090825     C     KEYTAB_C      KLIST
124202090825     C                   KFLD                    codut
124203090825     C                   KFLD                    KCOD
124204090825     C                   KFLD                    Kkey
124701090807     c*
124702090807     c* Caricamento schiera causali consegna anomala di dirottamento
124703090807     c                   clear                   ii
124704090807     c                   eval      kcod='7O'
124705090826     c     keytab        setll     TABEL00F
124706090826     c     keytab        reade     TABEL00F
124707090826     c                   dow       not %eof(TABEL00F)
124708090807     c                   movel     tbluni        ds7o
124709090807     c                   if        §7ofdi='D'
124710090807     c                   add       1             ii
124711090807     c                   movel     tblkey        s7o(ii)
124712090807     c                   endif
124713090826     c     keytab        reade     TABEL00F
124714090807     c                   enddo
124715091119     c* Caricamento schiera tipi bolla di recupero  e c/s
124716091103     C                   clear                   ii
124717091103     c                   eval      kcod='TB'
124718091103     C     keytab        SETLL     TABEL00F
124720091103     C     keytab        reade     TABEL00F
124721091103     c                   dow       not %eof(tabel00f)
124724091103     C     TBLFLG        IFNE      '*'
124725091103     C                   MOVEL     TBLUNI        DSTB
124726091103     C     §TBRBL        IFEQ      'R'
124727091119     C     §TBRBL        oreQ      'C'
124728091103     C                   ADD       1             ii
124729091103     C                   MOVEL     TBLKEY        TBO(ii)
124730091103     C                   ENDIF
124731091103     C                   ENDIF
124732091103     C     keytab        reade     TABEL00F
124733091103     C                   ENDDO
124734091216     c* Caricamento schiera sigle operatore tabella "3M"
124735091216     C                   clear                   ii
124736091216     c                   eval      kcod='3M'
124737091216     C     keytab        SETLL     TABEL00F
124738091216     C     keytab        reade     TABEL00F
124739091216     c                   dow       not %eof(tabel00f)
124744091216     C                   ADD       1             ii
124745091216     C                   MOVEL     TBLKEY        sop3m(ii)
124748091216     C     keytab        reade     TABEL00F
124749091216     C                   ENDDO
124800000000     C*
126300020910     C*--------------------
126400020910     C* REPERISCE DATA/ORA CORRENTE
126500020910     C*--------------------
126600090825     C                   TIME                    dataiso
126601090825     c                   move      dataiso       datcor
126602090826     c                   move      dataiso       datadmy
126603090826     c                   move      datadmy       data
126604090826     c                   TIME                    ora
130500000000     C* CARICA ORGANIGRAMMA
130600000000     C                   EXSR      CARORG
130601090825     c*
130602090825     c* Pulizia file wfdir prima di scriverlo
130603090825     c                   call      'QCMDEXC'
130604090825     c                   parm      C_Cmd_ClrWF   Qcmd
130605090825     c                   parm                    Qlen
130606090825     c
130607090825     c                   open      WFDIR01L
130608091029
130609091029     c* Pulizia file wfdirs prima di scriverlo
130610091029     c                   call      'QCMDEXC'
130611091029     c                   parm      C_Cmd_ClrWF1  Qcmd
130612091029     c                   parm                    Qlen
130613091029     c
130614091029     c                   open      WFDIRS0F
130615091009     c*
130616091009     c                   open(E)   titas38c
130617091009     c                   if        not %open(titas38c)
130618091009     c                   eval      %subst(wtas38:7:4)='GRPS'
130619091009     c                   open      titas38c
130620091009     c                   endif
130621091203      *
130622091203     c                   open(E)   titas30c
130623091203     c                   if        not %open(titas30c)
130624091203     c                   eval      %subst(wtas30:7:4)='GRPS'
130625091203     c                   open      titas30c
130626091203     c                   endif
131900000000     C*
132000000000     C                   ENDSR
132100000000     C*------------------------------------------------------------------------*
132200000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
132300000000     C*------------------------------------------------------------------------*
132400000000     C     CARORG        BEGSR
132500000000     C*
132600000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
132800090825     C     *loval        SETLL     AZORG01L
132900090825     C                   READ      AZORG01L
133000090825     C                   Z-ADD     0             I                 3 0
133001090825     C                   Z-ADD     0             k                 3 0
133002091106     C                   Z-ADD     0             y                 3 0
133100090825    1C                   DOW       not %eof(azorg01l)
133300090825    2C     ORGFAG        IFNE      'V'
133400000000     C                   ADD       1             I
133500000000     C                   Z-ADD     ORGFIL        FIL(I)
133600000000     C                   MOVEL     ORGDES        DFIL(I)
133601090825     c                   movel     orgfl3        div(I)
133602090825     c                   z-add     orgcar        are(I)
133700040308     C                   MOVEL     ORGDE3        OG143
133801090825    3c                   if        §ogntw='FED'
133802090824     c                   ADD       1             K
133803090824     c                   Z-ADD     orgfil        filfed(k)
133804090825    3c                   endif
133805091106    3c                   if        §ogntw='DPD'
133806091106     c                   ADD       1             y
133807091106     c                   Z-ADD     orgfil        fildpd(y)
133808091106    3c                   endif
133900090825    2C                   END
134100090825     C                   READ      AZORG01L
134200090825    1C                   ENDdo
134300000000     C*
134400000000     C                   ENDSR
134500000000     O*------------------------------------------------------------------------*
134600090825     Oqsysprt   E            finel          1 02
134700090825     O                                            8 'TIST66R '
134800090826     O                                           46 '***      STATISTICA  '
134900090826     O                                           67 'DIROTTAMENTI    **** '
135200090826     O                       DATA               110 '  /  /  '
135400090826     O                       ORA                119 '  :  :  '
135500090826     O                                          124 'Pag.'
135600090826     O                       PAGE          Z    128
149600000000     O*------------------------
149700090825     O          E            finel          3
149800090825     O                                          110 '*** FINE ELABORAZIONE ***'
