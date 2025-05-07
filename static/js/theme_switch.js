function oppositeTheme(theme) {
    if (theme === "light") {
        return "dark";
    } else {
        return "light";
    }
}

function themeIcon(theme) {
    if (theme === "dark") {
        return { src: '/black_moon.svg', alt: 'Dark Mode' };
    } else {
        return { src: '/white_sun.svg', alt: 'Light Mode' };
    }
}

function applySyntaxTheme(theme) {
    const lightSyntax = document.getElementById('light-syntax');
    const darkSyntax = document.getElementById('dark-syntax');

    if (theme === 'dark') {
        lightSyntax.disabled = true;
        darkSyntax.disabled = false;
    } else {
        lightSyntax.disabled = false;
        darkSyntax.disabled = true;
    }
}

document.addEventListener('DOMContentLoaded', function () {
    // ✅ Load saved theme or system preference
    const savedTheme = localStorage.getItem('theme');
    const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
    const initialTheme = savedTheme || (prefersDark ? 'dark' : 'light');
    const initialIcon = themeIcon(initialTheme);
    console.log(`Initial Theme: ${initialTheme}`)
    applySyntaxTheme(initialTheme);
    document.documentElement.setAttribute('data-theme', initialTheme);
    
    const toggleSwitch = document.getElementById('theme-switch');
    const img = toggleSwitch.querySelector('img');

    img.src = initialIcon.src;
    img.alt = initialIcon.alt;
    applySyntaxTheme(initialTheme);

    function switchTheme(e) {
        const currentTheme = document.documentElement.getAttribute('data-theme') || 'light';
        const newTheme = oppositeTheme(currentTheme);
        const icon_attrs = themeIcon(newTheme);

        document.documentElement.setAttribute('data-theme', newTheme);
        localStorage.setItem('theme', newTheme);  // ✅ Save theme
        img.src = icon_attrs.src;
        img.alt = icon_attrs.alt;
        applySyntaxTheme(newTheme);
    }

    toggleSwitch.addEventListener('click', switchTheme, false);
});
