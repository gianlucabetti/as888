000100940912     H*------------------------------------------------------------------------*
000200160919     H* SALVATAGGIO E PULIZIA VARIAZIONI BOLLE unica
000300940912     H*------------------------------------------------------------------------*
000400160919     H DECEDIT('0,') DATEDIT(*DMY.)
000500160919     Ffiarbf2c  UF   E           K DISK
000501160919     FFIARBU1L  UF   E           K DISK
000600160919     Ftitas30c  iF   E           K DISK
000601160920     Ftabel00f  iF   E           K DISK
000602170314     Ffnblp01l  iF   E           K DISK    extfile(wlibfil) usropn
000603160919      *
000604160919      * DS PASSAGGIO PARAMETRI
000605160919      *
000700160920     D                 DS
000800160919     D  DATpul                 1      8  0                                      DATA PULIZIA
000801160919     d  dataas                        4s 0 overlay(datpul:1)
000802160920     d  datmgs                        4s 0 overlay(datpul:5)
000803160919
000900160919     D KPJBA         E DS
000901160920      *
000902160920     D DS5AS1        E DS
000903160919
000904160919     d keyspe          ds
000905160919     d  arbaas
000906160919     d  arblnp
000907160919     d  arbnrs
000908160919     d  arbnsp
000909160919
000910160920     d savspe          s                   like(keyspe)
000911160919     d wok             s              1
000912160920     d codut           s              1  0 inz(1)
000913160920     d cod             s                   like(tblcod)
000914160920     d key             s                   like(tblkey)
000915160920     d w_datiso        s               d   datfmt(*iso)
000916160919
000917170314     D Wlibfil         S             21
000918170314     D Wlibfilprd      S             21    inz('FILTRAPRD/        ')
000919170314     D Wlibfil201      S             21    INZ('FILTRA201/        ')
000920170314
000921160919     IFNARBD00      01
000922160919     IFNARBK00      02
000923160919     IFIARBT00      03
000924160919     IFNARBP00      04
000925160919     IFNARBG00      05
000926160919     IFNARBM00      06
000927160919     IFNARBV00      07
000928160919     IFNARBN00      08
001000160919      **************************************************************************
001100160919     C     *ENTRY        PLIST
001200160919     C                   PARM                    KPJBA
001301160919
001302160919     c     kspe          klist
001303160919     c                   kfld                    arbaas
001304160919     c                   kfld                    arblnp
001305160919     c                   kfld                    arbnrs
001306160919     c                   kfld                    arbnsp
001307160920
001308160920     C     KTAB          KLIST
001309160920     C                   KFLD                    CODUT
001310160920     C                   KFLD                    COD
001311160920     C                   KFLD                    KEY
001312160920
001314160920      * RECUPERO DA TABELLA 5A I PARAMETRI DI PULIZIA
001315160920      *
001316160920     C                   eval      cod='5A'
001317160920     C                   eval      key='SEDE1'
001318160920     c                   clear                   ds5as1
001319160920     C     KTAB          CHAIN     TABEL00F
001320160920     c                   if        %found(tabel00f)
001321160920     C                   eval      ds5as1=TBLUNI
001322160920     c                   endif
001323160920
001324160920     c                   if        §5as1var=0
001325160920     c                   z-add     16            §5as1var
001326160920     c                   endif
001330160920
001331160920     C                   TIME                    WTIME            14 0          ORA E DATA
001332160920     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
001333170314
001334170314      ** ?Apertura file
001335170314     c                   eval      wlibfil=wlibfil201
001336170314     c                   eval      %subst(wlibfil:11:8)='FNBLP01L'
001337170314     c                   open(e)   fnblp01l
001338170314     c                   if        not %open(fnblp01l)
001339170314     c                   eval      wlibfil=wlibfilprd
001340170314     c                   eval      %subst(wlibfil:11:8)='FNBLP01L'
001341170314     c                   open      fnblp01l
001342170314     c                   endif
001343160920
001344160920     c* in base ai mesi calcolo la data di pulizia
001345160920     c                   z-add     §5as1var      ms                2 0
001346160920     c     *eur          move      wdate         w_datiso
001347160920     c* sottraggo i mesi alla data corrente
001348160920     c     w_datiso      subdur    ms:*months    W_datiso
001349160920     c                   move      W_datiso      datpul
001350160920     c* vado a inizio mese
001351160920     c                   move      01            datpul
001352160919
001353160919     c                   clear                   savspe
001400160919     C     *LOVAL        SETLL     fiarbf2c
001401160920     c                   setoff                                       010203
001402160920     c                   setoff                                       040506
001403160920     c                   setoff                                       0708
001500160919     c                   read      fiarbf2c
001600160920     c                   dow       not %eof(fiarbf2c) and arbaas<=dataas
001601160921     c***                if        arbaas=dataas
001602160919     c                   exsr      sr_chkspe
001603160921     c***                else
001604160921     c***                eval      wok='S'
001605160921     c***                endif
001606160919     c                   if        wok='S'
001607160919     c   01              delete    fnarbd00
001608160919     c   02              delete    fnarbk00
001609160919      * Variazioni di tassazione
001610160919     c                   if        *in03
001611160919     c                   if        arbsv3<>' '
001612160919     c     kspe          setll     fiarbu1l
001613160919     c     kspe          reade     fiarbu1l
001614160919     c                   dow       not %eof(fiarbu1l)
001615160919     c                   delete    fiarbu00
001616160919     c     kspe          reade     fiarbu1l
001617160920     c                   enddo
001618160920     c                   endif
001619160919     c                   delete    fiarbt00
001620160919     c                   endif
001621160919     c   04              delete    fnarbp00
001622160919     c   05              delete    fnarbg00
001623160919     c   06              delete    fnarbm00
001624160919     c   07              delete    fnarbv00
001625160919     c   08              delete    fnarbn00
001626160919     c                   endif
001627160920     c                   setoff                                       010203
001628160920     c                   setoff                                       040506
001629160920     c                   setoff                                       0708
001630160919     c                   read      fiarbf2c
001700160919     c                   enddo
001701160920     c                   seton                                        lr
001702160919     C*--------------------------------------------------------------*
001703160919     C*
001704160919     C*--------------------------------------------------------------*
001705160919     C     sr_chkspe     BEGSR
001800160919    2c                   if        keyspe<>savspe
001900160919     c                   eval      savspe=keyspe
002000160919     c                   eval      wok='S'
002100160920     c     kspe          chain     titas30c
002200160919    3c                   if        %found(titas30c)
002300160919    4c                   if        tasaas>dataas or
002400160919     c                             (tasaas=dataas and tasmgs>datmgs)
002401160921     c* se bolla non consegnata non pulisco le varizioni
002402160921     c                             or tasdcm=0
002500160919     c                   eval      wok='N'
002600160919    4c                   endif
002700160919   x3c                   else
002701170314     c* se non trovato titas la bolla potrebbe essere ancora "Da partire"
002702170314     c* (molto improbabile, ma potrebbe..)
002703170314     c     kspe          setll     fnblp01l
002704170314     c                   if        %equal(fnblp01l)
002800170314     c                   eval      wok='N'
002801170314     c                   endif
002900160919    3c                   endif
003000160919    2c                   endif
003100160919     c                   endsr
