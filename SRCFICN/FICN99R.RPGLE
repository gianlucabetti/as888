000001071120     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000002071120     H*PARMS ACTGRP(*caller)
000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300071120      * FICN99R - Stampa comunicazione fusione                       *
000500900606?     *--------------------------------------------------------------*
000501071120     fyupdc02l  if   e           k disk
000502071120     Fanrco98j  IF   E           K DISK
000503071120     Fficn55p   O    e             PRINTER usropn
000504071120     F                                     include(testal)
000505071120     f                                     rename(testal:testalA)
000506071120     Fficn65p   O    e             PRINTER usropn
000507071120     F                                     include(testal)
000508071120     f                                     rename(testal:testalC)
000509071120     Fficn68p   O    e             PRINTER usropn
000510071120     F                                     include(testal)
000511071120     f                                     rename(testal:testalD)
000512071120     Fficn96p   O    e             PRINTER usropn
000514071120     F                                     include(testal)
000515071120     f                                     rename(testal:testalO)
000918071120     D ficn99ds      e ds
000919071120     d capoconto       s                   like(rcokcc) inz('000400')
000920071120     d natura          s                   like(rcosnatura) inz('F')
000921071120     D soc001        E DS                  EXTNAME(xsoc001ds)
000922071120     D xsocds          DS          1000
000923071120     D KPJBA         E DS
001100020423     C     *ENTRY        PLIST                                                  *
001200071120     C                   PARM                    kpjba                          *
001400071120     c                   movel     kpjbu         ficn99ds
001402071120     C     Kpdc          KLIST
001403071120     C                   KFLD                    f99socn
001404071120     C                   KFLD                    capoconto
001405071120     C                   KFLD                    f99kscn
001406071120     C     Krco          KLIST
001407071120     C                   KFLD                    f99socn
001408071120     C                   KFLD                    natura
001409071120     C                   KFLD                    f99kscn
001410071120     c* se non trovo il fornitore nel tabellone oppure se il fornitore
001411071120     c* non ha variato la società, esco senza stampare la lettera
001412071120     c     kpdc          chain     yupdc02l
001413071120     c                   if        not %found(yupdc02l) or
001414071120     c                             (%found(yupdc02l) and pdcsocold=pdcsocnew)
001415071120     c                   seton                                        lr
001416071120     c                   return
001417071120     c                   else
001418071120     c* apro il printer file che mi interessa
001419071120     c                   select
001420071120     c* autotrasportatori
001421071120     c                   when      f99tip = 'A'
001422071120     c                   open      ficn55p
001423071120     c* coop
001424071120     c                   when      f99tip = 'C'
001425071120     c                   open      ficn65p
001426071120     c* Aff/def
001427071120     c                   when      f99tip = 'D'
001428071120     c                   open      ficn68p
001429071120     c*  prestazioni occasionali
001430071120     c                   when      f99tip = 'O'
001431071120     c                   open      ficn96p
001432071120     c                   endsl
001433071120     c* imposto dati atto notarile
001434071120     c                   select
001435071120     c* Me-vi Srl
001436071120     c                   when      f99socn = '137'
001437071120     c                   eval      nratto = 107476
001438071120     c                   eval      fasc02 = 28607
001439071120     c* SDM Srl
001440071120     c                   when      f99socn = '219'
001441071120     c                   eval      nratto = 107481
001442071120     c                   eval      fasc02 = 28612
001443071120     c* Abruzzese Trasporti Srl
001444071120     c                   when      f99socn = '211'
001445071120     c                   eval      nratto = 107477
001446071120     c                   eval      fasc02 = 28608
001447071120     c*  onica Trasporti Srl
001448071120     c                   when      f99socn = '212'
001449071120     c                   eval      nratto = 107478
001450071120     c                   eval      fasc02 = 28609
001451071120     c                   endsl
001452071120     C*  RICERCA società old
001454071120     C                   MOVEL     'SOC001'      TIPXSC
001455071120     C                   MOVEL     pdcsocold     SOCXSC
001456071120     C                   EXSR      REPSOC
001457071120     C     RTNXSC        IFNE      '1'
001458071120     C                   MOVEL     XSOCDS        SOC001
001459071120     c                   eval      desemio= xscrgs
001460071120     c                   eval      desemio1= %trimr(xscrgs) + ' è stata fusa -
001461071120     c                             nella:'
001462071120     c                   end
001464071120     C*  RICERCA società new
001465071120     C                   MOVEL     'SOC001'      TIPXSC
001466071120     C                   MOVEL     f99socn       SOCXSC
001467071120     C                   EXSR      REPSOC
001468071120     C     RTNXSC        IFNE      '1'
001469071120     C                   MOVEL     XSOCDS        SOC001
001470071120     c                   eval      desemin= xscrgs
001471071120     c                   eval      desemin1= xscrgs
001472071120     c                   eval      desemin2= xscrgs
001473071120     c                   eval      desemin3= %trimr(xscrgs) + ' subentra -
001474071120     c                             in tutti i diritti e doveri della'
001475071120     c                   eval      desemin4= %trimr(xscrgs) + '.'
001476071120     c                   eval      desemin5= xscrgs
001477071120     c                   eval      indemin= xscvia
001478071120     c                   eval      indemin2= xscvia
001479071120     c                   eval      capemin= xsccap
001480071120     c                   eval      capemin2= xsccap
001481071120     c                   eval      piemin= xsciva
001482071120     c                   eval      piemin2= xsciva
001483071120     c                   eval      cfemin= XSCCDF
001484071120     c                   eval      cfemin2= XSCCDF
001485071120     c                   if        xscprv = ' '
001486071120     C                   eval      locemin= xsccit
001487071120     C                   eval      locemin2= xsccit
001488071120     C                   ELSE
001489071120     C                   eval      locemin= %trimr(xsccit)+' ('+xscprv+')'
001490071120     C                   eval      locemin2= %trimr(xsccit)+' ('+xscprv+')'
001491071120     c                   end
001492071120     c                   end
001493071120     c* reperisce informazioni fornitore
001494071120     c                   exsr      srfor
001495071120     c* stampo lettera
001496071120     c                   select
001497071120     c* autotrasportatori
001498071120     c                   when      f99tip = 'A'
001499071120     c                   write     testala
001501071120     c* coop
001502071120     c                   when      f99tip = 'C'
001503071120     c                   write     testalc
001505071120     c* Aff/def
001506071120     c                   when      f99tip = 'D'
001507071120     c                   write     testald
001509071120     c*  prestazioni occasionali
001510071120     c                   when      f99tip = 'O'
001511071120     c                   write     testalo
001513071120     c                   endsl
001514071120     c                   end
001515020423     C*
003200071120     c                   seton                                        lr
003300071120     C*----------------------------------------------------*
003400071120     C* Reperimento dati società
003500071120     C*----------------------------------------------------*
003600071120     C     REPSOC        BEGSR
003700071120     C*
003800071120     C                   CALLB     'XSOC'
003900071120     C                   PARM                    TIPXSC            6
004000071120     C                   PARM                    SOCXSC            3
004100071120     C                   PARM                    CDSXSC            9 0
004200071120     C                   PARM                    MODXSC            3
004300071120     C                   PARM      *blanks       RTNXSC            1
004400071120     C                   PARM                    XSOCDS
004500071120     C                   PARM                    KPJBA
004600071120     C*
004700071120     C                   ENDSR
004800071120?     *--------------------------------------------------------------*
004900071120?     *  dati in testata                                             *
005000071120?     *--------------------------------------------------------------*
005100071120     C     srfor         BEGSR
005200071120      *
005500071120     C     Krco          CHAIN     anrco98j
007500071120     c*
007600071120     c                   if        %found(anrco98j)
007700071120     C                   MOVEL     sogdes        desfor
007800071120     C                   eval      indfor = indindriz
007900071120     c                   if        indprov <> ' '
008000071120     C                   eval      locfor = %trimr(indlocalit)+' ('+indprov+')'
008100071120     c                   else
008200071120     C                   eval      locfor = indlocalit
008300071120     c                   end
008400071120     C                   eval      capfor = indcap
008603071120     c                   end
018700071120      *
018800071120     C                   ENDSR
