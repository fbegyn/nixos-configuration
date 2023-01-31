<!DOCTYPE html>
<html lang="en-US" class="theme-auto">
<head>
	<meta charset="utf-8">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>nixos-configs/fish_right_prompt.fish at main - nixos-configs - tulpa.dev: git in plurality</title>
	<link rel="manifest" href="data:application/json;base64,eyJuYW1lIjoidHVscGEuZGV2OiBnaXQgaW4gcGx1cmFsaXR5Iiwic2hvcnRfbmFtZSI6InR1bHBhLmRldjogZ2l0IGluIHBsdXJhbGl0eSIsInN0YXJ0X3VybCI6Imh0dHBzOi8vdHVscGEuZGV2LyIsImljb25zIjpbeyJzcmMiOiJodHRwczovL3R1bHBhLmRldi9hc3NldHMvaW1nL2xvZ28ucG5nIiwidHlwZSI6ImltYWdlL3BuZyIsInNpemVzIjoiNTEyeDUxMiJ9LHsic3JjIjoiaHR0cHM6Ly90dWxwYS5kZXYvYXNzZXRzL2ltZy9sb2dvLnN2ZyIsInR5cGUiOiJpbWFnZS9zdmcreG1sIiwic2l6ZXMiOiI1MTJ4NTEyIn1dfQ==">
	<meta name="theme-color" content="#6cc644">
	<meta name="default-theme" content="auto">
	<meta name="author" content="cadey">
	<meta name="description" content="nixos-configs - redoing nixos configs for flakes">
	<meta name="keywords" content="go,git,self-hosted,gitea">
	<meta name="referrer" content="no-referrer">


	<link rel="alternate" type="application/atom+xml" title="" href="https://tulpa.dev/cadey/nixos-configs.atom">
	<link rel="alternate" type="application/rss+xml" title="" href="https://tulpa.dev/cadey/nixos-configs.rss">

	<link rel="icon" href="/assets/img/favicon.svg" type="image/svg+xml">
	<link rel="alternate icon" href="/assets/img/favicon.png" type="image/png">
	<link rel="stylesheet" href="/assets/css/index.css?v=1.18.1">
	
<script>
	window.addEventListener('error', function(e) {window._globalHandlerErrors=window._globalHandlerErrors||[]; window._globalHandlerErrors.push(e);});
	window.config = {
		appUrl: 'https:\/\/tulpa.dev\/',
		appSubUrl: '',
		assetVersionEncoded: encodeURIComponent('1.18.1'), 
		assetUrlPrefix: '\/assets',
		runModeIsProd:  true ,
		customEmojis: {"codeberg":":codeberg:","git":":git:","gitea":":gitea:","github":":github:","gitlab":":gitlab:","gogs":":gogs:"},
		useServiceWorker:  false ,
		csrfToken: 'j51oGLeYdJVNnLhcP-5VhD0w84M6MTY3NTEyNDQ3MDI1MjE3MDA2MA',
		pageData: {},
		requireTribute:  null ,
		notificationSettings: {"EventSourceUpdateTime":10000,"MaxTimeout":60000,"MinTimeout":10000,"TimeoutStep":10000}, 
		enableTimeTracking:  true ,
		
		mermaidMaxSourceCharacters:  5000 ,
		
		i18n: {
			copy_success: 'Copied!',
			copy_error: 'Copy failed',
			error_occurred: 'An error occurred',
			network_error: 'Network error',
		},
	};
	
	window.config.pageData = window.config.pageData || {};
</script>

	<noscript>
		<style>
			.dropdown:hover > .menu { display: block; }
			.ui.secondary.menu .dropdown.item > .menu { margin-top: 0; }
		</style>
	</noscript>

	
		<meta property="og:title" content="nixos-configs">
		<meta property="og:url" content="https://tulpa.dev/cadey/nixos-configs">
		
			<meta property="og:description" content="redoing nixos configs for flakes">
		
	
	<meta property="og:type" content="object">
	
		<meta property="og:image" content="https://tulpa.dev/avatars/4865b79fe980163ecaa6b49e68cb9fd5">
	

<meta property="og:site_name" content="tulpa.dev: git in plurality">

	<link rel="stylesheet" href="/assets/css/theme-auto.css?v=1.18.1">


</head>
<body>
	

	<div class="full height">
		<noscript>This website works better with JavaScript.</noscript>

		

		
			<div class="ui top secondary stackable main menu following bar light no-vertical-tabs">
				<div class="ui container" id="navbar">
	
	
	<div class="item brand" style="justify-content: space-between;">
		<a href="/" aria-label="Home">
			<img width="30" height="30" src="/assets/img/logo.svg" alt="Logo" aria-hidden="true">
		</a>
		
		<div class="ui basic icon button mobile-only" id="navbar-expand-toggle">
			<i class="sidebar icon"></i>
		</div>
	</div>

	
		<a class="item " href="/explore/repos">Explore</a>
	

	

	


	
		<a class="item" target="_blank" rel="noopener noreferrer" href="https://docs.gitea.io">Help</a>
		<div class="right stackable menu">
			
				<a class="item" href="/user/sign_up">
					<svg viewBox="0 0 16 16" class="svg octicon-person" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M10.5 5a2.5 2.5 0 1 1-5 0 2.5 2.5 0 0 1 5 0zm.061 3.073a4 4 0 1 0-5.123 0 6.004 6.004 0 0 0-3.431 5.142.75.75 0 0 0 1.498.07 4.5 4.5 0 0 1 8.99 0 .75.75 0 1 0 1.498-.07 6.005 6.005 0 0 0-3.432-5.142z"/></svg> Register
				</a>
			
			<a class="item" rel="nofollow" href="/user/login?redirect_to=%2fcadey%2fnixos-configs%2fsrc%2fbranch%2fmain%2fcommon%2fhome-manager%2ffish%2ffish_right_prompt.fish">
				<svg viewBox="0 0 16 16" class="svg octicon-sign-in" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M2 2.75C2 1.784 2.784 1 3.75 1h2.5a.75.75 0 0 1 0 1.5h-2.5a.25.25 0 0 0-.25.25v10.5c0 .138.112.25.25.25h2.5a.75.75 0 0 1 0 1.5h-2.5A1.75 1.75 0 0 1 2 13.25V2.75zm6.56 4.5 1.97-1.97a.75.75 0 1 0-1.06-1.06L6.22 7.47a.75.75 0 0 0 0 1.06l3.25 3.25a.75.75 0 1 0 1.06-1.06L8.56 8.75h5.69a.75.75 0 0 0 0-1.5H8.56z"/></svg> Sign In
			</a>
		</div>
	
</div>

			</div>
		



<div class="page-content repository file list ">
	<div class="header-wrapper">

	<div class="ui container">
		<div class="repo-header">
			<div class="repo-title-wrap df fc">
				<div class="repo-title">
					
					
						<div class="repo-icon mr-3">
	
		
			<svg viewBox="0 0 16 16" class="svg octicon-repo" width="32" height="32" aria-hidden="true"><path fill-rule="evenodd" d="M2 2.5A2.5 2.5 0 0 1 4.5 0h8.75a.75.75 0 0 1 .75.75v12.5a.75.75 0 0 1-.75.75h-2.5a.75.75 0 1 1 0-1.5h1.75v-2h-8a1 1 0 0 0-.714 1.7.75.75 0 0 1-1.072 1.05A2.495 2.495 0 0 1 2 11.5v-9zm10.5-1V9h-8c-.356 0-.694.074-1 .208V2.5a1 1 0 0 1 1-1h8zM5 12.25v3.25a.25.25 0 0 0 .4.2l1.45-1.087a.25.25 0 0 1 .3 0L8.6 15.7a.25.25 0 0 0 .4-.2v-3.25a.25.25 0 0 0-.25-.25h-3.5a.25.25 0 0 0-.25.25z"/></svg>
		
	
</div>

					
					<a href="/cadey">cadey</a>
					<div class="mx-2">/</div>
					<a href="/cadey/nixos-configs">nixos-configs</a>
					<a href="/cadey/nixos-configs.rss"><i class="ui grey icon tooltip ml-3" data-content="RSS Feed" data-position="top center"><svg viewBox="0 0 16 16" class="svg octicon-rss" width="18" height="18" aria-hidden="true"><path fill-rule="evenodd" d="M2.002 2.725a.75.75 0 0 1 .797-.699C8.79 2.42 13.58 7.21 13.974 13.201a.75.75 0 1 1-1.497.098 10.502 10.502 0 0 0-9.776-9.776.75.75 0 0 1-.7-.798zM2 13a1 1 0 1 1 2 0 1 1 0 0 1-2 0zm.84-5.95a.75.75 0 0 0-.179 1.489c2.509.3 4.5 2.291 4.8 4.8a.75.75 0 1 0 1.49-.178A7.003 7.003 0 0 0 2.838 7.05z"/></svg></i></a>
					<div class="labels df ac fw">
						
							
								
							
						
						
					</div>
				</div>
				
				
				
			</div>
			
				<div class="repo-buttons">
					
					<form method="post" action="/cadey/nixos-configs/action/watch?redirect_to=%2fcadey%2fnixos-configs%2fsrc%2fbranch%2fmain%2fcommon%2fhome-manager%2ffish%2ffish_right_prompt.fish">
						<input type="hidden" name="_csrf" value="j51oGLeYdJVNnLhcP-5VhD0w84M6MTY3NTEyNDQ3MDI1MjE3MDA2MA">
						<div class="ui labeled button tooltip" tabindex="0" data-content="Sign in to watch this repository." data-position="top center">
							<button type="submit" class="ui compact small basic button" disabled>
								<svg viewBox="0 0 16 16" class="svg octicon-eye" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M1.679 7.932c.412-.621 1.242-1.75 2.366-2.717C5.175 4.242 6.527 3.5 8 3.5c1.473 0 2.824.742 3.955 1.715 1.124.967 1.954 2.096 2.366 2.717a.119.119 0 0 1 0 .136c-.412.621-1.242 1.75-2.366 2.717C10.825 11.758 9.473 12.5 8 12.5c-1.473 0-2.824-.742-3.955-1.715C2.92 9.818 2.09 8.69 1.679 8.068a.119.119 0 0 1 0-.136zM8 2c-1.981 0-3.67.992-4.933 2.078C1.797 5.169.88 6.423.43 7.1a1.619 1.619 0 0 0 0 1.798c.45.678 1.367 1.932 2.637 3.024C4.329 13.008 6.019 14 8 14c1.981 0 3.67-.992 4.933-2.078 1.27-1.091 2.187-2.345 2.637-3.023a1.619 1.619 0 0 0 0-1.798c-.45-.678-1.367-1.932-2.637-3.023C11.671 2.992 9.981 2 8 2zm0 8a2 2 0 1 0 0-4 2 2 0 0 0 0 4z"/></svg>Watch
							</button>
							<a class="ui basic label" href="/cadey/nixos-configs/watchers">
								1
							</a>
						</div>
					</form>
					
						<form method="post" action="/cadey/nixos-configs/action/star?redirect_to=%2fcadey%2fnixos-configs%2fsrc%2fbranch%2fmain%2fcommon%2fhome-manager%2ffish%2ffish_right_prompt.fish">
							<input type="hidden" name="_csrf" value="j51oGLeYdJVNnLhcP-5VhD0w84M6MTY3NTEyNDQ3MDI1MjE3MDA2MA">
							<div class="ui labeled button tooltip" tabindex="0" data-content="Sign in to star this repository." data-position="top center">
								<button type="submit" class="ui compact small basic button" disabled>
									<svg viewBox="0 0 16 16" class="svg octicon-star" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8 .25a.75.75 0 0 1 .673.418l1.882 3.815 4.21.612a.75.75 0 0 1 .416 1.279l-3.046 2.97.719 4.192a.75.75 0 0 1-1.088.791L8 12.347l-3.766 1.98a.75.75 0 0 1-1.088-.79l.72-4.194L.818 6.374a.75.75 0 0 1 .416-1.28l4.21-.611L7.327.668A.75.75 0 0 1 8 .25zm0 2.445L6.615 5.5a.75.75 0 0 1-.564.41l-3.097.45 2.24 2.184a.75.75 0 0 1 .216.664l-.528 3.084 2.769-1.456a.75.75 0 0 1 .698 0l2.77 1.456-.53-3.084a.75.75 0 0 1 .216-.664l2.24-2.183-3.096-.45a.75.75 0 0 1-.564-.41L8 2.694v.001z"/></svg>Star
								</button>
								<a class="ui basic label" href="/cadey/nixos-configs/stars">
									0
								</a>
							</div>
						</form>
					
					
						<div class="ui labeled button
							
								tooltip disabled
							"
							
								data-content="Sign in to fork this repository."
							
						data-position="top center" tabindex="0">
							<a class="ui compact small basic button"
								
									
								
							>
								<svg viewBox="0 0 16 16" class="svg octicon-repo-forked" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M5 3.25a.75.75 0 1 1-1.5 0 .75.75 0 0 1 1.5 0zm0 2.122a2.25 2.25 0 1 0-1.5 0v.878A2.25 2.25 0 0 0 5.75 8.5h1.5v2.128a2.251 2.251 0 1 0 1.5 0V8.5h1.5a2.25 2.25 0 0 0 2.25-2.25v-.878a2.25 2.25 0 1 0-1.5 0v.878a.75.75 0 0 1-.75.75h-4.5A.75.75 0 0 1 5 6.25v-.878zm3.75 7.378a.75.75 0 1 1-1.5 0 .75.75 0 0 1 1.5 0zm3-8.75a.75.75 0 1 0 0-1.5.75.75 0 0 0 0 1.5z"/></svg>Fork
							</a>
							<div class="ui small modal" id="fork-repo-modal">
								<svg viewBox="0 0 16 16" class="close inside svg octicon-x" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M3.72 3.72a.75.75 0 0 1 1.06 0L8 6.94l3.22-3.22a.75.75 0 1 1 1.06 1.06L9.06 8l3.22 3.22a.75.75 0 1 1-1.06 1.06L8 9.06l-3.22 3.22a.75.75 0 0 1-1.06-1.06L6.94 8 3.72 4.78a.75.75 0 0 1 0-1.06z"/></svg>
								<div class="header">
									You&#39;ve already forked nixos-configs
								</div>
								<div class="content tl">
									<div class="ui list">
										
									</div>
									
								</div>
							</div>
							<a class="ui basic label" href="/cadey/nixos-configs/forks">
								0
							</a>
						</div>
					
				</div>
			
		</div>
	</div>

	<div class="ui tabs container">
		
			<div class="ui tabular stackable menu navbar">
				
				<a class="active item" href="/cadey/nixos-configs">
					<svg viewBox="0 0 16 16" class="svg octicon-code" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M4.72 3.22a.75.75 0 0 1 1.06 1.06L2.06 8l3.72 3.72a.75.75 0 1 1-1.06 1.06L.47 8.53a.75.75 0 0 1 0-1.06l4.25-4.25zm6.56 0a.75.75 0 1 0-1.06 1.06L13.94 8l-3.72 3.72a.75.75 0 1 0 1.06 1.06l4.25-4.25a.75.75 0 0 0 0-1.06l-4.25-4.25z"/></svg> Code
				</a>
				

				
					<a class=" item" href="/cadey/nixos-configs/issues">
						<svg viewBox="0 0 16 16" class="svg octicon-issue-opened" width="16" height="16" aria-hidden="true"><path d="M8 9.5a1.5 1.5 0 1 0 0-3 1.5 1.5 0 0 0 0 3z"/><path fill-rule="evenodd" d="M8 0a8 8 0 1 0 0 16A8 8 0 0 0 8 0zM1.5 8a6.5 6.5 0 1 1 13 0 6.5 6.5 0 0 1-13 0z"/></svg> Issues
						
					</a>
				

				

				
					<a class=" item" href="/cadey/nixos-configs/pulls">
						<svg viewBox="0 0 16 16" class="svg octicon-git-pull-request" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M7.177 3.073 9.573.677A.25.25 0 0 1 10 .854v4.792a.25.25 0 0 1-.427.177L7.177 3.427a.25.25 0 0 1 0-.354zM3.75 2.5a.75.75 0 1 0 0 1.5.75.75 0 0 0 0-1.5zm-2.25.75a2.25 2.25 0 1 1 3 2.122v5.256a2.251 2.251 0 1 1-1.5 0V5.372A2.25 2.25 0 0 1 1.5 3.25zM11 2.5h-1V4h1a1 1 0 0 1 1 1v5.628a2.251 2.251 0 1 0 1.5 0V5A2.5 2.5 0 0 0 11 2.5zm1 10.25a.75.75 0 1 1 1.5 0 .75.75 0 0 1-1.5 0zM3.75 12a.75.75 0 1 0 0 1.5.75.75 0 0 0 0-1.5z"/></svg> Pull Requests
						
					</a>
				

				

				
					<a href="/cadey/nixos-configs/projects" class=" item">
						<svg viewBox="0 0 16 16" class="svg octicon-project" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M1.75 0A1.75 1.75 0 0 0 0 1.75v12.5C0 15.216.784 16 1.75 16h12.5A1.75 1.75 0 0 0 16 14.25V1.75A1.75 1.75 0 0 0 14.25 0H1.75zM1.5 1.75a.25.25 0 0 1 .25-.25h12.5a.25.25 0 0 1 .25.25v12.5a.25.25 0 0 1-.25.25H1.75a.25.25 0 0 1-.25-.25V1.75zM11.75 3a.75.75 0 0 0-.75.75v7.5a.75.75 0 0 0 1.5 0v-7.5a.75.75 0 0 0-.75-.75zm-8.25.75a.75.75 0 0 1 1.5 0v5.5a.75.75 0 0 1-1.5 0v-5.5zM8 3a.75.75 0 0 0-.75.75v3.5a.75.75 0 0 0 1.5 0v-3.5A.75.75 0 0 0 8 3z"/></svg> Projects
						
					</a>
				

				
				<a class=" item" href="/cadey/nixos-configs/releases">
					<svg viewBox="0 0 16 16" class="svg octicon-tag" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M2.5 7.775V2.75a.25.25 0 0 1 .25-.25h5.025a.25.25 0 0 1 .177.073l6.25 6.25a.25.25 0 0 1 0 .354l-5.025 5.025a.25.25 0 0 1-.354 0l-6.25-6.25a.25.25 0 0 1-.073-.177zm-1.5 0V2.75C1 1.784 1.784 1 2.75 1h5.025c.464 0 .91.184 1.238.513l6.25 6.25a1.75 1.75 0 0 1 0 2.474l-5.026 5.026a1.75 1.75 0 0 1-2.474 0l-6.25-6.25A1.75 1.75 0 0 1 1 7.775zM6 5a1 1 0 1 0 0 2 1 1 0 0 0 0-2z"/></svg> Releases
					
				</a>
				

				
					<a class=" item" href="/cadey/nixos-configs/wiki" >
						<svg viewBox="0 0 16 16" class="svg octicon-book" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M0 1.75A.75.75 0 0 1 .75 1h4.253c1.227 0 2.317.59 3 1.501A3.744 3.744 0 0 1 11.006 1h4.245a.75.75 0 0 1 .75.75v10.5a.75.75 0 0 1-.75.75h-4.507a2.25 2.25 0 0 0-1.591.659l-.622.621a.75.75 0 0 1-1.06 0l-.622-.621A2.25 2.25 0 0 0 5.258 13H.75a.75.75 0 0 1-.75-.75V1.75zm8.755 3a2.25 2.25 0 0 1 2.25-2.25H14.5v9h-3.757c-.71 0-1.4.201-1.992.572l.004-7.322zm-1.504 7.324.004-5.073-.002-2.253A2.25 2.25 0 0 0 5.003 2.5H1.5v9h3.757a3.75 3.75 0 0 1 1.994.574z"/></svg> Wiki
					</a>
				

				
					<a class=" item" href="/cadey/nixos-configs/activity">
						<svg viewBox="0 0 16 16" class="svg octicon-pulse" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M6 2a.75.75 0 0 1 .696.471L10 10.731l1.304-3.26A.75.75 0 0 1 12 7h3.25a.75.75 0 0 1 0 1.5h-2.742l-1.812 4.528a.75.75 0 0 1-1.392 0L6 4.77 4.696 8.03A.75.75 0 0 1 4 8.5H.75a.75.75 0 0 1 0-1.5h2.742l1.812-4.529A.75.75 0 0 1 6 2z"/></svg> Activity
					</a>
				

				

				
			</div>
		
	</div>
	<div class="ui tabs divider"></div>
</div>

	<div class="ui container ">
		




		
		
		<div class="hide" id="validate_prompt">
			<span id="count_prompt">You can not select more than 25 topics</span>
			<span id="format_prompt">Topics must start with a letter or number, can include dashes (&#39;-&#39;) and can be up to 35 characters long.</span>
		</div>
		
		

		<div class="repo-button-row df ac sb fw">
			<div class="df ac">
				



<div class="fitted item choose reference">
	<div class="ui floating filter dropdown custom"
		data-branch-form=""
		data-can-create-branch="false"
		data-no-results="No results found."
		data-set-action="" data-submit-form=""
		data-view-type="branch"
		data-ref-name="main"
		data-branch-url-prefix="/cadey/nixos-configs/src/branch/"
		data-branch-url-suffix="/common/home-manager/fish/fish_right_prompt.fish"
		data-tag-url-prefix="/cadey/nixos-configs/src/tag/"
		data-tag-url-suffix="/common/home-manager/fish/fish_right_prompt.fish">
		<button class="branch-dropdown-button ellipsis ui basic small compact button df" @click="menuVisible = !menuVisible" @keyup.enter="menuVisible = !menuVisible">
			<span class="text df ac mr-2">
				
					<span :class="{visible: isViewTag}" v-if="isViewTag" v-cloak><svg viewBox="0 0 16 16" class="svg octicon-tag" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M2.5 7.775V2.75a.25.25 0 0 1 .25-.25h5.025a.25.25 0 0 1 .177.073l6.25 6.25a.25.25 0 0 1 0 .354l-5.025 5.025a.25.25 0 0 1-.354 0l-6.25-6.25a.25.25 0 0 1-.073-.177zm-1.5 0V2.75C1 1.784 1.784 1 2.75 1h5.025c.464 0 .91.184 1.238.513l6.25 6.25a1.75 1.75 0 0 1 0 2.474l-5.026 5.026a1.75 1.75 0 0 1-2.474 0l-6.25-6.25A1.75 1.75 0 0 1 1 7.775zM6 5a1 1 0 1 0 0 2 1 1 0 0 0 0-2z"/></svg></span>
					<span :class="{visible: isViewBranch}" v-if="isViewBranch" ><svg viewBox="0 0 16 16" class="svg octicon-git-branch" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M11.75 2.5a.75.75 0 1 0 0 1.5.75.75 0 0 0 0-1.5zm-2.25.75a2.25 2.25 0 1 1 3 2.122V6A2.5 2.5 0 0 1 10 8.5H6a1 1 0 0 0-1 1v1.128a2.251 2.251 0 1 1-1.5 0V5.372a2.25 2.25 0 1 1 1.5 0v1.836A2.492 2.492 0 0 1 6 7h4a1 1 0 0 0 1-1v-.628A2.25 2.25 0 0 1 9.5 3.25zM4.25 12a.75.75 0 1 0 0 1.5.75.75 0 0 0 0-1.5zM3.5 3.25a.75.75 0 1 1 1.5 0 .75.75 0 0 1-1.5 0z"/></svg></span>
					<span :class="{visible: isViewTree}" v-if="isViewTree" v-cloak><svg viewBox="0 0 16 16" class="svg octicon-git-branch" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M11.75 2.5a.75.75 0 1 0 0 1.5.75.75 0 0 0 0-1.5zm-2.25.75a2.25 2.25 0 1 1 3 2.122V6A2.5 2.5 0 0 1 10 8.5H6a1 1 0 0 0-1 1v1.128a2.251 2.251 0 1 1-1.5 0V5.372a2.25 2.25 0 1 1 1.5 0v1.836A2.492 2.492 0 0 1 6 7h4a1 1 0 0 0 1-1v-.628A2.25 2.25 0 0 1 9.5 3.25zM4.25 12a.75.75 0 1 0 0 1.5.75.75 0 0 0 0-1.5zM3.5 3.25a.75.75 0 1 1 1.5 0 .75.75 0 0 1-1.5 0z"/></svg></span>
					<strong ref="dropdownRefName" class="ml-3">main</strong>
				
			</span>
			<svg viewBox="0 0 16 16" class="dropdown icon svg octicon-triangle-down" width="14" height="14" aria-hidden="true"><path d="m4.427 7.427 3.396 3.396a.25.25 0 0 0 .354 0l3.396-3.396A.25.25 0 0 0 11.396 7H4.604a.25.25 0 0 0-.177.427z"/></svg>
		</button>
		<div class="data" style="display: none" data-mode="branches">
			
				
					<div class="item branch selected" data-url="main">main</div>
				
					<div class="item branch " data-url="lufta">lufta</div>
				
			
			
				
			
		</div>
		<div class="menu transition" :class="{visible: menuVisible}" v-if="menuVisible" v-cloak>
			<div class="ui icon search input">
				<i class="icon df ac jc m-0"><svg viewBox="0 0 16 16" class="svg octicon-filter" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M.75 3a.75.75 0 0 0 0 1.5h14.5a.75.75 0 0 0 0-1.5H.75zM3 7.75A.75.75 0 0 1 3.75 7h8.5a.75.75 0 0 1 0 1.5h-8.5A.75.75 0 0 1 3 7.75zm3 4a.75.75 0 0 1 .75-.75h2.5a.75.75 0 0 1 0 1.5h-2.5a.75.75 0 0 1-.75-.75z"/></svg></i>
				<input name="search" ref="searchField" autocomplete="off" v-model="searchTerm" @keydown="keydown($event)" placeholder="Filter branch or tag...">
			</div>
			
				<div class="header branch-tag-choice">
					<div class="ui grid">
						<div class="two column row">
							<a class="reference column" href="#" @click="createTag = false; mode = 'branches'; focusSearchField()">
								<span class="text" :class="{black: mode == 'branches'}">
									<svg viewBox="0 0 16 16" class="mr-2 svg octicon-git-branch" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M11.75 2.5a.75.75 0 1 0 0 1.5.75.75 0 0 0 0-1.5zm-2.25.75a2.25 2.25 0 1 1 3 2.122V6A2.5 2.5 0 0 1 10 8.5H6a1 1 0 0 0-1 1v1.128a2.251 2.251 0 1 1-1.5 0V5.372a2.25 2.25 0 1 1 1.5 0v1.836A2.492 2.492 0 0 1 6 7h4a1 1 0 0 0 1-1v-.628A2.25 2.25 0 0 1 9.5 3.25zM4.25 12a.75.75 0 1 0 0 1.5.75.75 0 0 0 0-1.5zM3.5 3.25a.75.75 0 1 1 1.5 0 .75.75 0 0 1-1.5 0z"/></svg>Branches
								</span>
							</a>
							
								<a class="reference column" href="#" @click="createTag = true; mode = 'tags'; focusSearchField()">
									<span class="text" :class="{black: mode == 'tags'}">
										<svg viewBox="0 0 16 16" class="mr-2 svg octicon-tag" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M2.5 7.775V2.75a.25.25 0 0 1 .25-.25h5.025a.25.25 0 0 1 .177.073l6.25 6.25a.25.25 0 0 1 0 .354l-5.025 5.025a.25.25 0 0 1-.354 0l-6.25-6.25a.25.25 0 0 1-.073-.177zm-1.5 0V2.75C1 1.784 1.784 1 2.75 1h5.025c.464 0 .91.184 1.238.513l6.25 6.25a1.75 1.75 0 0 1 0 2.474l-5.026 5.026a1.75 1.75 0 0 1-2.474 0l-6.25-6.25A1.75 1.75 0 0 1 1 7.775zM6 5a1 1 0 1 0 0 2 1 1 0 0 0 0-2z"/></svg>Tags
									</span>
								</a>
							
						</div>
					</div>
				</div>
			
			<div class="scrolling menu" ref="scrollContainer">
				<div v-for="(item, index) in filteredItems" :key="item.name" class="item" :class="{selected: item.selected, active: active == index}" @click="selectItem(item)" :ref="'listItem' + index">${ item.name }</div>
				<div class="item" v-if="showCreateNewBranch" :class="{active: active == filteredItems.length}" :ref="'listItem' + filteredItems.length">
					<a href="#" @click="createNewBranch()">
						<div v-show="createTag">
							<i class="reference tags icon"></i>
							Create tag <strong>${ searchTerm }</strong>
						</div>
						<div v-show="!createTag">
							<svg viewBox="0 0 16 16" class="svg octicon-git-branch" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M11.75 2.5a.75.75 0 1 0 0 1.5.75.75 0 0 0 0-1.5zm-2.25.75a2.25 2.25 0 1 1 3 2.122V6A2.5 2.5 0 0 1 10 8.5H6a1 1 0 0 0-1 1v1.128a2.251 2.251 0 1 1-1.5 0V5.372a2.25 2.25 0 1 1 1.5 0v1.836A2.492 2.492 0 0 1 6 7h4a1 1 0 0 0 1-1v-.628A2.25 2.25 0 0 1 9.5 3.25zM4.25 12a.75.75 0 1 0 0 1.5.75.75 0 0 0 0-1.5zM3.5 3.25a.75.75 0 1 1 1.5 0 .75.75 0 0 1-1.5 0z"/></svg>
							Create branch <strong>${ searchTerm }</strong>
						</div>
						<div class="text small">
							
								from &#39;main&#39;
							
						</div>
					</a>
					<form ref="newBranchForm" action="/cadey/nixos-configs/branches/_new/branch/main" method="post">
						<input type="hidden" name="_csrf" value="j51oGLeYdJVNnLhcP-5VhD0w84M6MTY3NTEyNDQ3MDI1MjE3MDA2MA">
						<input type="hidden" name="new_branch_name" v-model="searchTerm">
						<input type="hidden" name="create_tag" v-model="createTag">
						
							<input type="hidden" name="current_path" value="common/home-manager/fish/fish_right_prompt.fish">
						
					</form>
				</div>
			</div>
			<div class="message" v-if="showNoResults">${ noResults }</div>
		</div>
	</div>
</div>

				
				
				
				
				
				
					<span class="ui breadcrumb repo-path ml-2"><a class="section" href="/cadey/nixos-configs/src/branch/main" title="nixos-configs">nixos-configs</a><span class="divider">/</span><span class="section"><a href="/cadey/nixos-configs/src/branch/main/common" title="common">common</a></span><span class="divider">/</span><span class="section"><a href="/cadey/nixos-configs/src/branch/main/common/home-manager" title="home-manager">home-manager</a></span><span class="divider">/</span><span class="section"><a href="/cadey/nixos-configs/src/branch/main/common/home-manager/fish" title="fish">fish</a></span><span class="divider">/</span><span class="active section" title="fish_right_prompt.fish">fish_right_prompt.fish</span></span>
				
			</div>
			<div class="df ac">
				
				
				
				
			</div>
		</div>
		
			<div class="tab-size-8 non-diff-file-content">
	<h4 class="file-header ui top attached header df ac sb">
		<div class="file-header-left df ac pr-4">
			
				<div class="file-info text grey normal mono">
					
					
						<div class="file-info-entry">
							8 lines
						</div>
					
					
						<div class="file-info-entry">
							169 B
						</div>
					
					
				</div>
			
		</div>
		<div class="file-header-right file-actions df ac">
			
			
				<div class="ui buttons mr-2">
					<a class="ui mini basic button" href="/cadey/nixos-configs/raw/branch/main/common/home-manager/fish/fish_right_prompt.fish">Raw</a>
					
						<a class="ui mini basic button" href="/cadey/nixos-configs/src/commit/1cb5489db51aea742f20e8c8b707600354524636/common/home-manager/fish/fish_right_prompt.fish">Permalink</a>
					
					
						<a class="ui mini basic button" href="/cadey/nixos-configs/blame/branch/main/common/home-manager/fish/fish_right_prompt.fish">Blame</a>
					
					<a class="ui mini basic button" href="/cadey/nixos-configs/commits/branch/main/common/home-manager/fish/fish_right_prompt.fish">History</a>
					
				</div>
				<a download href="/cadey/nixos-configs/raw/branch/main/common/home-manager/fish/fish_right_prompt.fish"><span class="btn-octicon tooltip" data-content="Download file" data-position="bottom center"><svg viewBox="0 0 16 16" class="svg octicon-download" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M7.47 10.78a.75.75 0 0 0 1.06 0l3.75-3.75a.75.75 0 0 0-1.06-1.06L8.75 8.44V1.75a.75.75 0 0 0-1.5 0v6.69L4.78 5.97a.75.75 0 0 0-1.06 1.06l3.75 3.75zM3.75 13a.75.75 0 0 0 0 1.5h8.5a.75.75 0 0 0 0-1.5h-8.5z"/></svg></span></a>
				
					
						<span class="btn-octicon tooltip disabled" data-content="You must fork this repository to make or propose changes to this file." data-position="bottom center"><svg viewBox="0 0 16 16" class="svg octicon-pencil" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M11.013 1.427a1.75 1.75 0 0 1 2.474 0l1.086 1.086a1.75 1.75 0 0 1 0 2.474l-8.61 8.61c-.21.21-.47.364-.756.445l-3.251.93a.75.75 0 0 1-.927-.928l.929-3.25a1.75 1.75 0 0 1 .445-.758l8.61-8.61zm1.414 1.06a.25.25 0 0 0-.354 0L10.811 3.75l1.439 1.44 1.263-1.263a.25.25 0 0 0 0-.354l-1.086-1.086zM11.189 6.25 9.75 4.81l-6.286 6.287a.25.25 0 0 0-.064.108l-.558 1.953 1.953-.558a.249.249 0 0 0 .108-.064l6.286-6.286z"/></svg></span>
					
					
						<span class="btn-octicon tooltip disabled" data-content="You must have write access to make or propose changes to this file." data-position="bottom center"><svg viewBox="0 0 16 16" class="svg octicon-trash" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M6.5 1.75a.25.25 0 0 1 .25-.25h2.5a.25.25 0 0 1 .25.25V3h-3V1.75zm4.5 0V3h2.25a.75.75 0 0 1 0 1.5H2.75a.75.75 0 0 1 0-1.5H5V1.75C5 .784 5.784 0 6.75 0h2.5C10.216 0 11 .784 11 1.75zM4.496 6.675a.75.75 0 1 0-1.492.15l.66 6.6A1.75 1.75 0 0 0 5.405 15h5.19c.9 0 1.652-.681 1.741-1.576l.66-6.6a.75.75 0 0 0-1.492-.149l-.66 6.6a.25.25 0 0 1-.249.225h-5.19a.25.25 0 0 1-.249-.225l-.66-6.6z"/></svg></span>
					
				
			
		</div>
	</h4>
	<div class="ui attached table unstackable segment">
		
			
	


		
		<div class="file-view code-view">
			
				
				<table>
					<tbody>
						
						
						<tr>
							<td id="L1" class="lines-num"><span id="L1" data-line-number="1"></span></td>
							
							<td rel="L1" class="lines-code chroma"><code class="code-inner"><span class="k">function</span> <span class="nb">fish_right_prompt
</span></code></td>
						</tr>
						
						
						<tr>
							<td id="L2" class="lines-num"><span id="L2" data-line-number="2"></span></td>
							
							<td rel="L2" class="lines-code chroma"><code class="code-inner"><span class="nb">    </span><span class="k">set</span> <span class="na">-l</span> <span class="nv">st</span> <span class="nv">$status</span>
</code></td>
						</tr>
						
						
						<tr>
							<td id="L3" class="lines-num"><span id="L3" data-line-number="3"></span></td>
							
							<td rel="L3" class="lines-code chroma"><code class="code-inner">
</code></td>
						</tr>
						
						
						<tr>
							<td id="L4" class="lines-num"><span id="L4" data-line-number="4"></span></td>
							
							<td rel="L4" class="lines-code chroma"><code class="code-inner">    <span class="k">if</span> <span class="o">[</span> <span class="nv">$status</span> !<span class="o">=</span> <span class="m">0</span> <span class="o">]</span>
</code></td>
						</tr>
						
						
						<tr>
							<td id="L5" class="lines-num"><span id="L5" data-line-number="5"></span></td>
							
							<td rel="L5" class="lines-code chroma"><code class="code-inner">        <span class="k">echo</span> <span class="o">(</span><span class="nb">set_color</span> <span class="nv">$theme_color_error</span><span class="o">)</span> ↵ <span class="nv">$st</span><span class="o">(</span><span class="nb">set_color</span> <span class="nv">$theme_color_normal</span><span class="o">)</span>
</code></td>
						</tr>
						
						
						<tr>
							<td id="L6" class="lines-num"><span id="L6" data-line-number="6"></span></td>
							
							<td rel="L6" class="lines-code chroma"><code class="code-inner">    <span class="k">end</span>
</code></td>
						</tr>
						
						
						<tr>
							<td id="L7" class="lines-num"><span id="L7" data-line-number="7"></span></td>
							
							<td rel="L7" class="lines-code chroma"><code class="code-inner"><span class="k">end</span>
</code></td>
						</tr>
						
					</tbody>
				</table>
				<div class="code-line-menu ui vertical pointing menu tippy-target">
					
						<a class="item ref-in-new-issue" href="/cadey/nixos-configs/issues/new?body=https%3a%2f%2ftulpa.dev%2fcadey%2fnixos-configs%2fsrc%2fcommit%2f1cb5489db51aea742f20e8c8b707600354524636/common%2fhome-manager%2ffish%2ffish_right_prompt.fish" rel="nofollow noindex">Reference in new issue</a>
					
					<a class="item view_git_blame" href="https://tulpa.dev/cadey/nixos-configs/blame/commit/1cb5489db51aea742f20e8c8b707600354524636/common/home-manager/fish/fish_right_prompt.fish">View Git Blame</a>
					<a class="item copy-line-permalink" data-url="https://tulpa.dev/cadey/nixos-configs/src/commit/1cb5489db51aea742f20e8c8b707600354524636/common/home-manager/fish/fish_right_prompt.fish">Copy Permalink</a>
				</div>
				
			
		</div>
	</div>
</div>

		
	</div>
</div>


	

	</div>

	

	<footer>
	<div class="ui container">
		<div class="ui left">
			<a target="_blank" rel="noopener noreferrer" href="https://gitea.io">Powered by Gitea</a>
			
			
				Page: <strong>9ms</strong>
				Template: <strong>1ms</strong>
			
		</div>
		<div class="ui right links">
			
			<div class="ui language bottom floating slide up dropdown link item">
				<svg viewBox="0 0 16 16" class="svg octicon-globe" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M1.543 7.25h2.733c.144-2.074.866-3.756 1.58-4.948.12-.197.237-.381.353-.552a6.506 6.506 0 0 0-4.666 5.5zm2.733 1.5H1.543a6.506 6.506 0 0 0 4.666 5.5 11.13 11.13 0 0 1-.352-.552c-.715-1.192-1.437-2.874-1.581-4.948zm1.504 0h4.44a9.637 9.637 0 0 1-1.363 4.177c-.306.51-.612.919-.857 1.215a9.978 9.978 0 0 1-.857-1.215A9.637 9.637 0 0 1 5.78 8.75zm4.44-1.5H5.78a9.637 9.637 0 0 1 1.363-4.177c.306-.51.612-.919.857-1.215.245.296.55.705.857 1.215A9.638 9.638 0 0 1 10.22 7.25zm1.504 1.5c-.144 2.074-.866 3.756-1.58 4.948-.12.197-.237.381-.353.552a6.506 6.506 0 0 0 4.666-5.5h-2.733zm2.733-1.5h-2.733c-.144-2.074-.866-3.756-1.58-4.948a11.738 11.738 0 0 0-.353-.552 6.506 6.506 0 0 1 4.666 5.5zM8 0a8 8 0 1 0 0 16A8 8 0 0 0 8 0z"/></svg>
				<div class="text">glico</div>
				<div class="menu language-menu">
					
						<a lang="en-US" data-url="/?lang=en-US" class="item active selected">glico</a>
					
				</div>
			</div>
			<a href="/assets/js/licenses.txt">Licenses</a>
			<a href="/api/swagger">API</a>
			
		</div>
	</div>
</footer>




	<script src="/assets/js/index.js?v=1.18.1" onerror="alert('Failed to load asset files from ' + this.src + ', please make sure the asset files can be accessed and the ROOT_URL setting in app.ini is correct.')"></script>

</body>
</html>

