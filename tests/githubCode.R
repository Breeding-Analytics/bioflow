#
# # changing directory
# cd <path>
#
# # cloning a repository on your computer
# git clone <your_repo>
#
#
# # create a new brach 'create', switches to a new branch '-b'
# # 'create_user' is the name of the new branch
# git checkout -b create_user
#
# # to check the branches you have
# git branch
#
# # to switch between branches
# git switch <branch_name>
#
# # pull the most recent version of the repo (where main is the main branch)
# git pull origin main
#
# # add your changes
# git add --all
#
# # commit the changes
# git commit -m "your text"
#
# # check the status
# git status
#
# # push the changes if you are in a branch
# git push --set-upstream origin create_user
# # push changes if you are the main branch : git push -u origin main
#
#
# # You can now navigate to the repository on your GitHub webpage
# # https://github.com/Breeding-Analytics/bioflow
# # and toggle to the branch you pushed to see the changes you have made in-browser.
# # make the pull request and merge
#
# #####
#
# ## reset to the previous commit (the 1 can be a greater number to go to an earlier version)
# git reset --hard HEAD~1
#
# # other potentially useful code
# git config --global --edit
# git commit --amend --reset-author
#
