AssignStim <- function(dat, trial, env = parent.frame(n = 2)) {
  # data
  fix <- dat$item[[trial]]$fix
  stimmat <- dat$item[[trial]]$meta$stimmat

  if (env$exp$setup$font$right == T) {
    fix$xs <- env$exp$setup$display$resolutionX - fix$xs
  }

  # drift correct
  # --------------
  if (is.null(dat$item[[trial]]$meta$drift) == T) {
    dat$item[[trial]]$meta$drift <- NA
  }
  # x axis
  if (env$exp$setup$assign$driftX == T) {
    if (is.na(dat$item[[trial]]$meta$drift) == F) {
      fix$xn <- fix$xs - dat$item[[trial]]$meta$drift.x
    } else {
      fix$xn <- fix$xs
    }
  } else {
    fix$xn <- fix$xs
  }

  # y axis
  if (env$exp$setup$assign$driftY == T) {
    if (is.na(dat$item[[trial]]$meta$drift) == F) {
      fix$yn <- fix$ys - dat$item[[trial]]$meta$drift.y + env$exp$setup$font$height / 2
    } else {
      fix$yn <- fix$ys
    }
  } else {
    fix$yn <- fix$ys
  }

  # check outlier
  # --------------

  if (env$exp$setup$assign$outlier == T) {
    fix <- CheckOutlier(fix, stimmat, env$exp$setup$assign$outlierDist)
  } else {
    fix$type <- "in"
  }

  if (mean(fix$type == "in") < .1) {
    dat$item[[trial]]$fix <- NULL
    return(dat)
  }

  # move fixations
  # ---------------

  if (env$exp$setup$assign$moveMethod == "hit") {
    if (env$exp$setup$assign$moveY == T) {
      fix <- MoveFixationsY(fix, stimmat)
    }

    if (env$exp$setup$assign$moveX == T) {
      fix <- MoveFixationsX(fix, stimmat)
    }
  }

  if (env$exp$setup$assign$moveMethod == "area") {
    if (env$exp$setup$assign$moveY == T) {
      moveY <- TRUE
    } else {
      moveY <- FALSE
    }

    if (env$exp$setup$assign$moveX == T) {
      moveX <- TRUE
    } else {
      moveX <- FALSE
    }

    fix <- MoveFixations(fix, stimmat, x.adj = moveX, y.adj = moveY)
  }


  # line assignment
  # ----------------
  # warp method
  if (env$exp$setup$assign$lineMethod == "warp") {
    
    # extract xy position of fixation and words and y position of lines
    fixation_XY <- fix[c("xn", "yn")]
    word_XY <- data.frame(cbind(
      tapply(stimmat$xm, stimmat$ianum, mean), 
      tapply(stimmat$ym, stimmat$ianum, mean)
    ))
    
    fix$line <- as.numeric(as.factor(Warp(fixation_XY, word_XY)$yn))
    fix$run <- NA
    fix$linerun <- NA
    
  }

  # map letter and IA
  # ------------------

  fix$subid <- stimmat$subid[1]
  fix$trialid <- stimmat$trialid[1]
  fix$trialnum <- stimmat$trialnum[1]
  fix$itemid <- stimmat$itemid[1]
  fix$cond <- stimmat$cond[1]

  fix$letternum <- NA
  fix$letter <- NA
  fix$wordnum <- NA
  fix$word <- NA
  fix$sentnum <- NA
  fix$sent <- NA
  fix$sent.nwords <- NA
  fix$ianum <- NA
  fix$ia <- NA

  if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
    fix$target <- NA
  }

  fix$line.let <- NA
  fix$word.land <- NA
  fix$ia.land <- NA
  fix$line.word <- NA
  fix$sent.word <- NA

  fix$trial.nwords <- NA
  fix$trial <- NA

  for (i in 1:nrow(fix)) {
    # i <- 1

    if (fix$type[i] == "in" & fix$line[i] > 0 & is.na(fix$line[i]) == F) {
      selected_stimmat <- stimmat[stimmat$line == fix$line[i], ]

      out <- abs(fix$xn[i] - selected_stimmat$xm)

      fix$letternum[i] <- selected_stimmat$letternum[which.min(out)]
      fix$letter[i] <- selected_stimmat$letter[which.min(out)]
      fix$wordnum[i] <- selected_stimmat$wordnum[which.min(out)]
      fix$word[i] <- selected_stimmat$word[which.min(out)]
      fix$sentnum[i] <- selected_stimmat$sentnum[which.min(out)]
      fix$sent[i] <- selected_stimmat$sent[which.min(out)]
      fix$sent.nwords[i] <- selected_stimmat$sent.nwords[which.min(out)]
      fix$ianum[i] <- selected_stimmat$ianum[which.min(out)]
      fix$ia[i] <- selected_stimmat$ia[which.min(out)]

      if (env$exp$setup$type == "target" | env$exp$setup$type == "boundary" | env$exp$setup$type == "fast") {
        fix$target[i] <- selected_stimmat$target[which.min(out)]
      }

      fix$line.let[i] <- selected_stimmat$letline[which.min(out)]
      fix$word.land[i] <- selected_stimmat$letword[which.min(out)]
      fix$ia.land[i] <- selected_stimmat$letia[which.min(out)]
      fix$line.word[i] <- selected_stimmat$wordline[which.min(out)]
      fix$sent.word[i] <- selected_stimmat$wordsent[which.min(out)]

      fix$trial.nwords[i] <- selected_stimmat$trial.nwords[which.min(out)]
      fix$trial[i] <- selected_stimmat$trial[which.min(out)]
    }
  }


  # align fixations on y axis
  # -------------------------

  for (i in 1:max(stimmat$line)) {
    fix$ym[fix$line == i & is.na(fix$line) == F] <- stimmat$ym[stimmat$line == i][1]
  }


  # return
  # -------

  if (env$exp$setup$font$right == T) {
    fix$xs <- env$exp$setup$display$resolutionX - fix$xs
    fix$xn <- env$exp$setup$display$resolutionX - fix$xn
    dat$item[[trial]]$meta$stimmat$xsn <- env$exp$setup$display$resolutionX - dat$item[[trial]]$meta$stimmat$xs
    dat$item[[trial]]$meta$stimmat$xen <- env$exp$setup$display$resolutionX - dat$item[[trial]]$meta$stimmat$xe
    dat$item[[trial]]$meta$stimmat$xs <- dat$item[[trial]]$meta$stimmat$xen
    dat$item[[trial]]$meta$stimmat$xe <- dat$item[[trial]]$meta$stimmat$xsn
    dat$item[[trial]]$meta$stimmat$xsn <- NULL
    dat$item[[trial]]$meta$stimmat$xen <- NULL
    dat$item[[trial]]$meta$stimmat$xm <- (dat$item[[trial]]$meta$stimmat$xs + dat$item[[trial]]$meta$stimmat$xe) / 2
  }

  dat$item[[trial]]$fix <- fix[is.na(fix$type) == F, ]

  return(dat)
}

assign_stimulus <- function(fixations_dataframe, characters_dataframe, resolutionX, trial_dict, lineMethod) {

  # x axis
  fixations_dataframe$xn <- fixations_dataframe$xs

  # y axis
  fixations_dataframe$yn <- fixations_dataframe$ys

  # check outlier
  # --------------

  # compute text field coordinates
  left <- min(characters_dataframe$xs)
  right <- max(characters_dataframe$xe)
  top <- min(characters_dataframe$ys)
  bottom <- max(characters_dataframe$ye)
  
  fixations_dataframe$type <- "in"
  
  fixations_dataframe$type[fixations_dataframe$xn < left - (right - left)*crit] <- "out"
  fixations_dataframe$type[fixations_dataframe$xn > right + (right - left)*crit] <- "out"
  fixations_dataframe$type[fixations_dataframe$yn < top - (bottom - top)*crit] <- "out"
  fixations_dataframe$type[fixations_dataframe$yn > bottom + (bottom - top)*crit] <- "out"

  if (mean(fixations_dataframe$type == "in") < .1) {
    fixations_dataframe<- NULL
    return(fixations_dataframe)
  }

  fixations_dataframe$run <- NA
  fixations_dataframe$linerun <- NA

  # map letter and IA (interest area, usually words)
  # ------------------

  fixations_dataframe$subid <- characters_dataframe$subid[1]
  fixations_dataframe$trialid <- characters_dataframe$trialid[1]
  fixations_dataframe$trialnum <- characters_dataframe$trialnum[1]
  fixations_dataframe$itemid <- characters_dataframe$itemid[1]
  fixations_dataframe$cond <- characters_dataframe$cond[1]

  fixations_dataframe$letternum <- NA
  fixations_dataframe$letter <- NA
  fixations_dataframe$wordnum <- NA
  fixations_dataframe$word <- NA
  fixations_dataframe$sentnum <- NA
  fixations_dataframe$sent <- NA
  fixations_dataframe$sent.nwords <- NA
  fixations_dataframe$ianum <- NA
  fixations_dataframe$ia <- NA

  fixations_dataframe$line.let <- NA
  fixations_dataframe$word.land <- NA
  fixations_dataframe$ia.land <- NA
  fixations_dataframe$line.word <- NA
  fixations_dataframe$sent.word <- NA

  fixations_dataframe$trial.nwords <- NA
  fixations_dataframe$trial <- NA

  for (i in 1:nrow(fixations_dataframe)) {

    if (fixations_dataframe$type[i] == "in" & fixations_dataframe$line[i] > 0 & is.na(fixations_dataframe$line[i]) == F) {
      selected_characters_dataframe <- characters_dataframe[characters_dataframe$line == fixations_dataframe$line[i], ]

      out <- abs(fixations_dataframe$xn[i] - selected_characters_dataframe$xm)

      fixations_dataframe$letternum[i] <- selected_characters_dataframe$letternum[which.min(out)]
      fixations_dataframe$letter[i] <- selected_characters_dataframe$letter[which.min(out)]
      fixations_dataframe$wordnum[i] <- selected_characters_dataframe$wordnum[which.min(out)]
      fixations_dataframe$word[i] <- selected_characters_dataframe$word[which.min(out)]
      fixations_dataframe$sentnum[i] <- selected_characters_dataframe$sentnum[which.min(out)]
      fixations_dataframe$sent[i] <- selected_characters_dataframe$sent[which.min(out)]
      fixations_dataframe$sent.nwords[i] <- selected_characters_dataframe$sent.nwords[which.min(out)]
      fixations_dataframe$ianum[i] <- selected_characters_dataframe$ianum[which.min(out)]
      fixations_dataframe$ia[i] <- selected_characters_dataframe$ia[which.min(out)]
      fixations_dataframe$line.let[i] <- selected_characters_dataframe$letline[which.min(out)]
      fixations_dataframe$word.land[i] <- selected_characters_dataframe$letword[which.min(out)]
      fixations_dataframe$ia.land[i] <- selected_characters_dataframe$letia[which.min(out)]
      fixations_dataframe$line.word[i] <- selected_characters_dataframe$wordline[which.min(out)]
      fixations_dataframe$sent.word[i] <- selected_characters_dataframe$wordsent[which.min(out)]
      fixations_dataframe$trial.nwords[i] <- selected_characters_dataframe$trial.nwords[which.min(out)]
      fixations_dataframe$trial[i] <- selected_characters_dataframe$trial[which.min(out)]
    }
  }


  # align fixations on y axis
  # -------------------------

  for (i in 1:max(characters_dataframe$line)) {
    fixations_dataframe$ym[fixations_dataframe$line == i & is.na(fixations_dataframe$line) == F] <- characters_dataframe$ym[characters_dataframe$line == i][1]
  }

  return(fixations_dataframe)
}