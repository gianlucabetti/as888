000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020926     H OPTION(*NODEBUGIO)
000300900606?     *--------------------------------------------------------------*
000400021126      * CNC0W2R  annullamento quadratura incassi giornalieri         *
000700991029?     *--------------------------------------------------------------*
000800910521      * 03 -FINE LAVORO                                              *
000900910521      * 06 -AGGIORNAMENTO                                            *
001100900606?     *--------------------------------------------------------------*
001200021126     FCNC0W2d   CF   E             WORKSTN
001201030318     Fazorg01l  iF   E           k disk
001404021126     Ffnqig02L  uF   E           K DISK
001405021126     Ffnqig01L  iF   E           K DISK    rename(fnqig000:fnqig001)
004300991029?     *--------------------------------------------------------------*
004301021126     D kpjba         e ds
008205030404     D msg             S             60    DIM(3) CTDATA PERRCD(1)              SCHIERA ER
008206021126     D savdta          S                   like(qigdta)                         DS3A TIPO
008207021126     D savimp          S                   like(qigimp)                         DS3A TIPO
008208021126     D dtaiso          S               d   inz                                  DS3A TIPO
008209021126     D dtaeur          S               d   datfmt(*eur) inz                     DS3A TIPO
008300991029?     *--------------------------------------------------------------*
008400991029      * CICLO PRINCIPALE
008500991029?     *--------------------------------------------------------------*
008501030404     c* controllo password
008502030404     c                   do        *hival
008503030404     C                   EXFMT     CN0W101
008504030404     c                   clear                   $msg
008505030404     c   kc              leave
008506030411     c                   if        passv = 'ONELIO'
009001021115     c                   do        *hival
010000021126     C                   EXFMT     CN0W102
010201021125     C   kc              leave
010202030318     c* controlli
010203030318     c                   exsr      ctr01
010204030318     c   20              iter
010237021126     c                   if        *inkf and dtav <> 0
010243021115     c                   exsr      aggio
010244021115     c                   leave
010245021115     c                   end
016400991102     C                   ENDDO
016401030404     c                   else
016402030404     c                   movel     msg(3)        $msg
016403030404     C                   END
016404030404     c                   if        *inkc or *inkf
016405030404     c                   leave
016406030404     C                   END
016407030404     C                   ENDDO
017100991029     C*
017200910521     C                   SETON                                        LR
017211021115     C**-------------------------------------------------------------
017212030318     C** controlli
017213021115     C**-------------------------------------------------------------
017214030318     C     ctr01         BEGSR
017215021126     c                   setoff                                       20
017216021126     c                   clear                   $msg
017217030506     c                   clear                   SAVDTA
017218030506     c                   clear                   SAVIMP
017219030318     c* controllo po
017220030318     c     pov           chain     azorg01l
017221030318     c                   if        not %found(azorg01l)
017222030318     c                   seton                                        2001
017223030318     c                   else
017224030318     c*
017225021126     c                   move      amgudt        qigdta
017244021126     c*
017245021126     c     kqig          setgt     fnqig02l
017246021126     c                   do        *hival
017247030318     c     pov           readpe    fnqig02l
017248021126     c                   if        %eof(fnqig02l)
017249021126     c                   if        savdta = 0
017250021126     c                   seton                                        20
017251021126     c                   movel     msg(2)        $msg
017253021126     c                   end
017254021126     c                   leave
017255021126     c                   else
017256021126     c* memorizzo i dati del primo record non annullato. Se il primo
017257021126     c* record è annullato emmetto l'errore
017258021126     c                   if        savdta = 0
017259021126     c                   if        qigann = ' '
017260021126     c                   move      qigdta        savdta
017261021126     c                   move      qigimp        savimp
017262021126     c                   iter
017263021126     c                   else
017264021126     c                   seton                                        20
017265021126     c                   movel     msg(1)        $msg
017266021126     c                   end
017267021126     c                   else
017268021126     c* se il secondo record è annullato controllo che esista anche il
017269021126     c* corrispondente valido. Se non esiste do errore
017270021126     c                   if        qigann = 'A'
017271021126     c                   clear                   qigann
017272021126     c     kqig2         chain     fnqig01l
017273021126     c                   if        not %found(fnqig01l)
017274021126     c                   seton                                        20
017275021126     c                   movel     msg(1)        $msg
017276021126     c                   end
017277021126     c                   end
017278021126     c                   end
017279021126     c                   leave
017280021126     c                   end
017281021126     c                   enddo
017282030318     c                   end
017284021126     c*
017285021126     c                   if        not *in20
017286021126     c                   move      savdta        dtaiso
017287021126     c                   move      dtaiso        dtaeur
017288021126     c                   move      dtaeur        dtav
017289021126     c                   z-add     savimp        impv
017290021126     c                   else
017291021126     c                   z-add     0             dtav
017292021126     c                   z-add     0             impv
017293021126     c                   end
017294021115     c                   endsr
017416021120     C**-------------------------------------------------------------
017417021120     C** aggiorna
017418021120     C**-------------------------------------------------------------
017419021120     C     aggio         BEGSR
017420021126     c                   clear                   qigann
017422021126     c     kqig1         chain     fnqig02l
017423021126     c                   if        %found(fnqig02l)
017530021126     c                   move      'A'           qigann
017536021126     c                   update    fnqig000
017537021126     c                   end
017538021120     c                   endsr
048700991102     C*-----------------------------------------------------------*
048800991029     C*  OPERAZIONI INIZIALI
048900991029     C*-----------------------------------------------------------*
049000941124     C     *INZSR        BEGSR
049100991029     C*
049200000000     C     *ENTRY        PLIST
049300000000     C                   PARM                    KPJBA
049301030318     c*
051000021126     C                   move      UDATE         dtaeur
051001021126     C                   move      dtaeur        dtaiso
051500021126     C                   move      dtaiso        AMGUDT            8 0
053400900515      *---------------------------------------------------------------*
053500900515      * CHIAVI                                                        *
053600900515      *---------------------------------------------------------------*
053700021119     C     Kqig          KLIST
053801030318     C                   KFLD                    pov
054000021126     C                   KFLD                    qigdta
054001021126     C     Kqig1         KLIST
054002030318     C                   KFLD                    pov
054003021126     C                   KFLD                    savdta
054004021126     C                   KFLD                    qigann
054005021126     C     Kqig2         KLIST
054006021126     C                   KFLD                    qigann
054007030318     C                   KFLD                    pov
054008021126     C                   KFLD                    qigdta
062800991102     C*
062900991102     C                   ENDSR
063000021126** MSG
063100021126Non è possibile eseguire l'annullamento. Ne esiste già uno!!
063200021126Non esiste nulla da annullare.
063300030404Password non corretta
