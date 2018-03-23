000100980521      ****************************************************************
000200980521      *  RIEPILOGO INDICATORI
000300981008      ****************************************************************
000400990317      ****************************************************************
000500980521     H DECEDIT('0,') DATEDIT(*DMY.)
000600980623
000700990602      *------------------------------------------------------------------------*
001000150122     ffnblt01l  IF   E           K DISK
001001150123     fazorg01l  IF   E           K DISK
001900150122     ffisgn00f  uf a E           k DISK
002000980521      *------------------------------------------------------------------------*
002100980702
002200990602      * Definizione campi di work ------------------------------------------
002300121029     d i               s              3  0 inz
002400121029     d Primo           s              4  0 inz
002500121030     d Qlen            s             15  5 inz(%len(Qcmd))
002600121030     d Qcmd            s             80    inz
002700121030     d $Finerec        s              1    inz(*off)
002701150122     d wncl            s                   like(blpncd)
002702150122     d wnsc            s                   like(blpncd)
002703150122     d kwho            s                   like(sgnwho)
002704150123     d wnc1            s                   like(§ldnc1)
002800990601
002900990602      * DS esterne ---------------------------------------------------------
003000990602
003100150122     D fnlsw01ds     E DS
003201150122     d lnp                            3  0 dim(6) overlay(fnlsw01ds:34)
003202150122     d tfp                            3  0 dim(6) overlay(fnlsw01ds:52)
003300990602
003400990602     D KPJBA         E DS
003401150122     D DSBLDE        E DS
003402150122     D DSBLD         E DS
003403150123     D og143         E DS
003500980525      *
003600020911     D*-------------
003700020911     D* DS DI WRK
003800020911     D*-------------
003900121029     d fnblpds       e ds                  extname(fnblp00f) inz
003901150122     D DSBLD7        E DS
003902150122     D  §C                     1     17  0
003903150122     D                                     DIM(17)
003904150122     D                 DS
003905150122     D  §CB                    1     18  0
003906150122     D                                     DIM(18)
003907150122     D  §CA                    1     18
003908150122     D                                     DIM(18)
003909150122     D                 DS
003910150122     D  §2                     1      2  0
003911150122     D                                     DIM(2)
003912150122     D  §2A                    1      2
003913150122     D                                     DIM(2)
004000121029
004100121029     D WrkStringaSql   S           4500
004200121029     D                                     VARYING
004300150122     D WrkLnpIn        S           4000
004400121029     D                                     VARYING
004401150122     IFISGN000
004402150122     I              SGNFLS                      §LDLNP
004403150122     I              SGNDLP                      §LDDLP
004404150122     I              SGNCDP                      §LDCDP
004405150122     I              SGNNRS                      §LDNRS
004406150122     I              SGNNCD                      §LDNCD
004407150122     I              SGNLNA                      §LDLNA
004408150122     I              SGNDLA                      §LDDLA
004409150122     I              SGNTNA                      §LDTNA
004410150122     I              SGNTSP                      §LDTSP
004411150122     I              SGNZSC                      §LDZSC
004412150122     I              SGNNC1                      §LDNC1
004413150122     I              SGNNC2                      §LDNC2
004414150122     I              SGNRSM                      §LDRSM
004415150122     I              SGNPRM                      §LDPRM
004416150122     I              SGNRSD                      §LDRSD
004417150122     I              SGNCAD                      §LDCAD
004418150122     I              SGNIND                      §LDIND
004419150122     I              SGNLOD                      §LDLOD
004420150122     I              SGNPRD                      §LDPRD
004421150122     I              SGNCPD                      §LDCPD
004422150122     I              SGNNCP                      §LDNCP
004423150122     I              SGNNCU                      §LDNCU
004424150122     I              SGNRST                      §LDRST
004425150122     I              SGNVOL                      §LDVOL
004426150122     I              SGNPKG                      §LDPKG
004427150122     I              SGNFVR                      §LDFVR
004428150122     I              SGNAAS                      §LDAAS
004429150122     I              SGNMGS                      §LDMGS
004430150122     I              SGNTST                      §DETST
004431150122     I              SGNTSE                      §DETSE
004432150122     I              SGNSSE                      §DESSE
004433150122     I              SGNSIP                      §DESIP
004434150122     I              SGNSIA                      §DESIA
004435150122     I              SGNAWB                      §DEAWB
004436150122     I              SGNISA                      §DEISA
004437150122     I              SGNFCA                      §DEFCA
004438150122     I              SGNEUR                      §DEEUR
004439150122     I              SGNXX1                      §DEXX1
004440150122     I              SGNXX2                      §DEXX2
004800990603
004900980521      *
005000121029     C     *entry        plist
005100121029     C                   parm                    kpjba
005200150122     C                   movel     kpjbu         fnlsw01ds
005300121030
006000121029      /free
006100121029         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
006200121030
006300150122       //
006400121029       exsr sr_prepsql;
006500121029       // elaboro dati estratti
006600121029       exsr sr_elabora;
006700121030
007200980623
007300121029       *inlr=*on;
007400121029       //-------------------------------------------------------------*
007500150122       //Preparazione stringa sql per elaborazione bolle part.        *
007600121029       //-------------------------------------------------------------*
007700121029       Begsr SR_prepSql;
007800121029
007900121029       WrkStringaSql='select * from fnblp00f' +
008000121029                     ' where' +
008100150122                     ' blpdim between ' +
008101150122                     %char(lswdti) + ' and ' +
008102150122                     %char(lswdtf) + ' and blpnrs=40 and ' +
008200150122                     'blplnp in ('     ;
008500150122                     exsr sr_whrlnp;
008600150122          WrkStringaSql=WrkStringaSql+WrklnpIn;
009000121029
009100121029       endsr;
009200121029       //-------------------------------------------------------------*
009300150122       Begsr SR_whrlnp ;
009400121029          Primo= 1 ;
009500121029
009600150122          reset WrkLnpIn;
009700150122          for I=1 to 6 ;
009800150122             if lnp(i)=0;
009900121029                leave;
010000121029             endif;
010100121029             if primo>1;
010200150122                WrkLnpIn=WrkLnpIn + ' , ';
010300121029             endif;
010400150122             WrkLnpIn=WrkLnpIn + %char(Lnp(i));
010500121029             Primo=primo+1   ;
010600121029          endfor;
010700150122          WrkLnpIn=WrkLnpIn + ')';
010800121029       endsr;
010900121029       //-------------------------------------------------------------*
011000121029       Begsr SR_Elabora;
011100121029        $finerec=*off;
011200121029        exec sql prepare STRINGASQL from :WrkStringaSql;
011300121029        exec sql declare BLPCsr cursor for StringaSql;
011400121029        exec sql open BLPcsr;
011500121029        dow $finerec=*off;
011600121029           exec sql Fetch BLPcsr into :fnblpds ;
011700121029           if sqlcod=100 or sqlcod<0;
011800121029              $finerec = *on;
011900121029              leave;
012000121029           endif;
012001150122
012002150122                §lclnp=blpfls;
012003150122                §lcnrs=blpnrs;
012004150122                §lczsc=blpznc;
012005150122                §lclna=blplna;
012006150123                clear wnc1;
012007150122           if blpfns='S';
012008150122             setll (blpaas:blplnp:blpnrs:blpnsp) fnblt01l;
012009150122             reade (blpaas:blplnp:blpnrs:blpnsp) fnblt01l;
012010150122             dow   not %eof(fnblt01l)  ;
012011150122                §lcncd=bltnsc ;
012012150122                EXSR  M10 ;
012013150122      /end-free
012014150122     C                   MOVEA     §CA           kwho
012015150122      /free
012016150122           exsr sr_fisgn;
012017150122                reade (blpaas:blplnp:blpnrs:blpnsp) fnblt01l;
012018150122             enddo;
012019150122           else;
012020150122             wncl=blpncd;
012021150122             if blpnca=0;
012022150122                blpnca=blpncd;
012023150122             endif;
012024150122             dow wncl<=blpnca;
012025150122                §lcncd=wncl   ;
012026150122                exsr M10 ;
012027150122      /end-free
012028150122     C                   MOVEA     §CA           Kwho
012029150122      /free
012030150122           exsr sr_fisgn;
012031150122                wncl+=1;
012032150122             enddo;
012033150122           endif;
012034150122
012900121029        enddo;
013000121030        exec sql close BLPcsr;
013100121030       endsr;
013101150122      /end-free
013102150122     C*****************************************************
013103150122     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
013104150122     C*****************************************************
013105150122     C     M10           BEGSR
013106150122     C                   MOVE      *ZERO         §TT               3 0
013107150122     C                   MOVE      *ZERO         §TT2              3 0
013108150122     C                   MOVE      *ZERO         §1                1 0
013109150122     C                   Z-ADD     18            §S                2 0
013110150122     C                   DO        17            §N                2 0
013111150122     C                   SUB       1             §S
013112150122     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
013113150122     C                   MOVE      §C(§N)        §CB(§N)
013114150122     C     §N            DIV       2             §CTR              3 0
013115150122     C                   MVR                     §RST              3 030
013116150122     C     *IN(30)       IFEQ      '1'
013117150122     C     §C(§S)        MULT      2             §N2               2 0
013118150122     C                   MOVE      §N2           §A2               2
013119150122     C                   MOVEA     §A2           §2A
013120150122     C                   XFOOT     §2            §CTR
013121150122     C                   ELSE
013122150122     C                   Z-ADD     §C(§S)        §CTR
013123150122     C                   END
013124150122     C                   ADD       §CTR          §TT
013125150122     C                   END
013126150122     C*
013127150122     C     §TT           ADD       9             §TT2
013128150122     C                   MOVE      0             §TT2
013129150122     C     §TT2          SUB       §TT           §1
013130150122     C*
013131150122     C                   MOVE      §1            §CB(18)
013132150122     C                   ENDSR
013133150122     C*****************************************************
013134150122     C*
013135150122     C*****************************************************
013136150122     C     sr_fisgn      BEGSR
013137150122     c                   clear                   dsbld
013138150122     c                   clear                   dsblde
013139150122     c                   clear                   fisgn000
013140150122     C                   CLEAR                   SGNATB
013141150122     c
013142150122     C                   MOVEL(P)  KWHO          SGNWHO
013144150122     C                   MOVEL     lswsts        SGNST1
013145150122     C                   CLEAR                   SGNST2
013146150122     C                   MOVE      '0'           SGNFL1
013147150122     C     BLPCCM        IFGT      0
013148150122     C                   MOVEL     BLPCCM        SGNKSC
013149150122     C                   ELSE
013150150122     C                   MOVEL     BLPKSC        SGNKSC
013151150122    4C                   ENDIF
013152150122     C                   MOVEL     BLPLNP        SGNLNP
013153150122     C                   MOVEL     BLPNSP        SGNNSP
013154150122     C                   CLEAR                   SGNT6A
013155150122     C                   CLEAR                   SGNT6B
013156150122     C                   CLEAR                   SGNT6C
013157150122     C                   CLEAR                   SGNT6D
013158150122     C                   CLEAR                   SGNT6E
013159150122     C                   CLEAR                   SGNT6F
013160150122     c                   eval      sgndatora = %char(%timestamp:*iso0)
013161150122     c                   z-add     blpaas        §ldaas
013162150122     c                   z-add     blpmgs        §ldmgs
013163150122     c                   z-add     blpnrs        §ldnrs
013164150122     c                   z-add     §lcncd        §ldncd
013165150123     c                   z-add     blpfls        §ldlnp
013166150123     c                   z-add     blplna        §ldlna
013167150123     c     blplna        chain     azorg01l
013168150123     c                   if        %found(azorg01l)
013169150123     c                   movel     orgde3        og143
013170150123     c                   if        orgde5<>*blanks
013171150123     C                   MOVEL     ORGDE5        §lddla
013172150123     c                   else
013173150123     C                   MOVEL     ORGDES        §lddla
013174150123     c                   endif
013175150123     c                   endif
013176150123     c                   z-add     blptfa        §ldtna
013177150123     c                   move      blptsp        §ldtsp
013178150123     c                   move      blpznc        §ldzsc
013179150123     c                   eval      wnc1+=1
013180150123     c                   eval      §ldnc1=wnc1
013181150123     c                   z-add     blpncl        §ldnc2
013182150123     C                   MOVEL     BLPRSM        §LDRSM
013183150123     C                   MOVEL     BLPPRM        §LDPRM
013184150123     C                   MOVEL     BLPRSD        §LDRSD
013185150123     C                   MOVEL     BLPCAD        §LDCAD
013186150123     C                   MOVEL     BLPIND        §LDIND
013187150123     C                   MOVEL     BLPLOD        §LDLOD
013188150123     C                   MOVE      BLPPRD        §LDPRD
013189150123     C                   MOVE      BLPPDR        §LDCPD
013190150123     C                   MOVEL     BLPVLF        §LDVOL
013191150123     C                   MOVEL     BLPPKF        §LDPKG
013192150123     C                   MOVEL     BLPFVF        §LDFVR
013193150123     C                   MOVEL     'ITA'         §DETST
013194150123     c                   if        §ogntw='EEX'
013195150123     c                   movel     'EEX'         §detse
013196150123     c                   else
013197150123     c                   clear                   §detse
013198150123     c                   endif
013199150122     c* loop per i tfp richiesti
013200150122     c                   z-add     1             i
013201150122     c                   dow       i<=6
013202150122     c                   if        tfp(i)>0
013203150122     c                   z-add     tfp(i)        sgntnp
013204150122     c                   write(e)  fisgn000
013205150122     c                   endif
013206150122     c                   add       1             i
013207150122     c                   enddo
013208150122
013209150122     c                   endsr
013327150122      /free
