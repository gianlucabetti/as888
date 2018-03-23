000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200051219     H* TNTU98R *----------------------------------------------------*
000300051219     H* - LEGGE fogli viaggio partenza - FNFGV
000400051220     H* - LEGGE fogli viaggio partenza in arrivo - FNfVA
000500000000     H*--------------------------------------------------------------*
000600051214     ftntud01l  uf a e           k disk
000700051219     fFNFgV02L  if   e           k Disk
000800051220     fFNfva05L  if   e           k Disk
000900051215     d
001000051214     dkpjba          e ds
001100051214     dtntu05ds       e ds
001200060103     dtntudtads      e ds
001300051215     d
001400051219     d Kdata           s                   Like(fgvdfv)
001500051219     d Kfgs            s                   Like(fgvlnp)
001600051215     d Kmult           s                   Like(tudmult)
001700051215     d Kcli            s                   Like(tudcli) inz(0)
001800051215     d codut           s              1  0 inz(1)
001900051215     d Dataiso         s               d   datfmt(*iso)
002000051220     d
002100051220     Ifnfva000
002200051220     I              fvaatb                      fgvatb
002300051220     I              fvalai                      fgvlnp
002400051220     I              fvadfa                      fgvdfv
002500030722     F**
002600900518     C***************************************************************
002700051214     c* Moltiplicatori calcolati:
002800051219     c*    010 - fogli viaggio partenza
002900051220     c*    020 - fogli viaggio partenza in arrivo
003000051214     c
003100051215     c                   if        t05mult<>*blanks
003200051220     c                   z-add     2             contamult         3 0
003300051215     c                   else
003400051215     c                   z-add     1             contamult         3 0
003500051215     c                   endif
003600051214     c
003700051220    1c                   dow       contamult<=2
003800051215     c                   exsr      TipoMult
003900051220     c
004000051220     c                   if        t05fil=0
004100051220     c                   clear                   kfgs
004200051220     c                   endif
004300051219     c
004400060102     c* Cerco i p.o. perchè la lf è per linea /data e non viceversa
004500051219     c
004600051219     c                   if        t05fil=0
004700051220     c
004800051220     c                   if        kmult='010'
004900051219     c     kfgs          setgt     fnfgv02l
005000051219     c                   read      fnfgv02l
005100051220     c                   else
005200051220     c     kfgs          setgt     fnfva05l
005300051220     c                   read      fnfva05l
005400051220     c                   endif
005500051220     c
005600051219     c                   else
005700051220     c                   if        kmult='010'
005800051219     c     kfgs          setll     fnfgv02l
005900051219     c     kfgs          reade     fnfgv02l
006000051220     c                   else
006100051220     c     kfgs          setll     fnfva05l
006200051220     c     kfgs          reade     fnfva05l
006300051220     c                   endif
006400051220     c
006500051219     c                   endif
006600051219     c
006700051220    2c                   dow       not %eof
006800051219     c                   z-add     fgvlnp        kfgs
006900051219     c                   clear                   contafgv          9 0
007000051215     c
007100051215     c* Imposta il giro sulla data
007200051215     c                   z-add     t05dti        kdata
007300051219    3c                   dow       kdata<=t05dtf
007400051215     c
007500051220     c                   if        kmult='010'
007600051219     c     kfgv          setll     fnfgv02l
007700051219     c     kfgv          reade     fnfgv02l
007800051220     c                   else
007900051220     c     kfgv          setll     fnfva05l
008000051220     c     kfgv          reade     fnfva05l
008100051220     c                   endif
008200051220
008300051220    4c                   dow       not %eof
008400051215     c
008500051215     c* Escludo gli annullati
008600051219    5c                   if        fgvatb=' '
008700051219     c                   add       1             contafgv
008800051219    5c                   endif
008900051219
009000051220     c                   if        kmult='010'
009100051219     c     kfgv          reade     fnfgv02l
009200051220     c                   else
009300051220     c     kfgv          reade     fnfva05l
009400051220     c                   endif
009500051220     c
009600051219    4c                   enddo
009700051219     c
009800051215     c                   exsr      SCriviTUD
009900051215     c
010000051219     c                   clear                   contafgv
010100051219     c
010200051219     c* Aggiungo 1 giorno alla data
010300051219     c     *iso          movel     kdata         dataiso
010400051219     c                   adddur    1:*D          dataiso
010500051219     c                   movel     dataiso       kdata
010600051219    3c                   enddo
010700051215     c
010800051219     c                   if        t05fil=0
010900051220     c
011000051220     c                   if        kmult='010'
011100051219     c     kfgs          setgt     fnfgv02l
011200051219     c                   read      fnfgv02l
011300051220     c                   else
011400051220     c     kfgs          setgt     fnfva05l
011500051220     c                   read      fnfva05l
011600051220     c                   endif
011700051220     c
011800051219     c                   else
011900051219     c* Se impostato un p.o. finito la lettura
012000051219     c                   leave
012100051219     c                   endif
012200051219     c
012300051219    2c                   enddo
012400051215     c
012500051215     c                   add       1             contamult
012600051215     c                   enddo
012700060103     c
012800060103     c* Se non richiesto un moltiplicatore o un p.o., aggiorno la tabella
012900060103     c*  TUP
013000060103     c                   if        t05mult=*blanks and t05fil=0
013100060103     c                   clear                   tntudtads
013200060103     c                   eval      §DTAazn = kcoaz
013300060103     c                   eval      §DTAdta = t05dtf
013400060103     c                   call      'TNTUDTAR'
013500060103     c                   parm                    tntudtads
013600060103     c                   endif
013700060103     c
013800941216     C*
013900051123     c
014000051123     C                   SETON                                        LR
014100991206     C**************************************************************************
014200051123     C     *INZSR        BEGSR
014300051214     c
014400051214     C     *ENTRY        PLIST
014500051214     C                   PARM                    kpjba
014600051214     c                   movel     kpjbu         tntu05ds
014700051123     c
014800051219     c     kfgv          Klist
014900051219     c                   Kfld                    kfgs
015000051214     c                   Kfld                    kdata
015100051215     c     ktud          Klist
015200051215     c                   Kfld                    kmult
015300051215     c                   Kfld                    dataiso
015400051219     c                   Kfld                    kfgs
015500051215     c                   Kfld                    kcli
015600051214     c
015700051219     c                   if        t05fil>0
015800051219     c                   eval      kfgs=t05fil
015900051219     c                   endif
016000051219     c
016100051124    cc                   endsr
016200051214     C**************************************************************************
016300051214     c* Imposto i campi in base al tipo moltiplicatore
016400051214     C**************************************************************************
016500051215     C     TIPOMULT      BEGSR
016600051215     c
016700051215     c                   select
016800051220     c* Numero fogli viaggio partenza
016900051219     c                   when      contamult=1 or t05mult='010'
017000051219     c                   eval      kmult='010'
017100051220     c* Numero fogli viaggio partenza abbinati
017200051220     c                   when      contamult=2 or t05mult='020'
017300051220     c                   eval      kmult='020'
017400051215     c                   endsl
017500051219     c                   ENDSR
017600051215     C**************************************************************************
017700051215     c* Scrivo il file TNTUD00F
017800051215     C**************************************************************************
017900051215     C     SCriviTUD     BEGSR
018000051215     c     *iso          movel     kdata         dataiso
018100051215     c
018200051219     c                   if        contafgv>0
018300051215     c
018400051215     c     ktud          chain     tntud01l
018500051215     c
018600051219     c                   z-add     contafgv      tudqta
018700051215     c
018800051215     c                   if        not %found(tntud01l)
018900051215     c                   eval      tuddta=dataiso
019000051215     c                   eval      tudmult=kmult
019100051219     c                   eval      tudfil=kfgs
019200051215     c                   clear                   tudcli
019300051215     c                   write     tntud000
019400051215     c                   else
019500051215     c                   update    tntud000
019600051215     c                   endif
019700051215     c                   endif
019800051215     c
019900051215     c                   ENDSR
